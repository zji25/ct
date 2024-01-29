import pandas as pd
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.pipeline import make_pipeline
from sklearn.ensemble import RandomForestClassifier
from sklearn.naive_bayes import MultinomialNB
from sklearn.neighbors import KNeighborsClassifier
from sklearn.linear_model import RidgeClassifier
from sklearn.metrics import classification_report
from sklearn.preprocessing import LabelEncoder
import torch
from torch import nn, optim


def main():
    file_path = 'output/comments_w_sentiments.csv'
    df = pd.read_csv(file_path)
    X, y = df['comment'], df['sentiment']
    simple_classifiers(X, y)
    sentiment_classifier(X, y)


def simple_classifiers(X, y):
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

    models = {
        'kneighborsclassifier': (
            KNeighborsClassifier(), {'n_neighbors': [3, 5, 7]}
        ),
        'ridgeclassifier': (
            RidgeClassifier(), {'alpha': [0.1, 0.5, 1.0]}
        ),
        'multinomialnb': (
            MultinomialNB(), {'alpha': [0.1, 0.5, 1.0]}
        ),
        'randomforestclassifier': (
            RandomForestClassifier(),
            {'n_estimators': [50, 100, 200], 'max_depth': [None, 10, 20]}
        ),
    }
    for model_name, (model, param_grid) in models.items():
        text_clf = make_pipeline(CountVectorizer(), model)
        prefixed_param_grid = {f'{model_name}__{param}': values for param, values in param_grid.items()}
        grid_search = GridSearchCV(text_clf, prefixed_param_grid, cv=5, scoring='accuracy')
        grid_search.fit(X_train, y_train)
        print(f'best parameters for {model_name}: {grid_search.best_params_}')
        predictions = grid_search.predict(X_test)
        report = classification_report(y_test, predictions, zero_division=1)
        print(f'classification report for {model_name}:')
        print(report)
        print('=' * 60)


class SentimentClassifier(nn.Module):
    def __init__(self, vocab_size, hidden_size, output_size):
        super(SentimentClassifier, self).__init__()
        self.embedding = nn.Embedding(vocab_size, hidden_size)
        self.lstm = nn.LSTM(hidden_size, hidden_size)
        self.fc = nn.Linear(hidden_size, output_size)

    def forward(self, x):
        x = self.embedding(x)
        x, _ = self.lstm(x)
        x = x[:, -1, :]
        output = self.fc(x)
        return output


def sentiment_classifier(X, y):
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
    vectorizer = CountVectorizer()
    vectorizer.fit(X)
    vocab_size = len(vectorizer.vocabulary_)
    X_train = vectorizer.transform(X_train).toarray()
    X_test = vectorizer.transform(X_test).toarray()
    hidden_size = 50
    output_size = len(y_train.unique())
    model = SentimentClassifier(vocab_size, hidden_size, output_size)
    criterion = nn.CrossEntropyLoss()
    optimizer = optim.Adam(model.parameters(), lr=0.01)
    X_train_tensor = torch.from_numpy(X_train).long()
    X_test_tensor = torch.from_numpy(X_test).long()
    label_encoder = LabelEncoder()
    y_train = label_encoder.fit_transform(y_train)
    y_test = label_encoder.transform(y_test)
    y_train_tensor = torch.tensor(y_train).long()
    epochs = 2
    for epoch in range(epochs):
        optimizer.zero_grad()
        outputs = model(X_train_tensor)
        loss = criterion(outputs, y_train_tensor)
        loss.backward()
        optimizer.step()
        with torch.no_grad():
            test_outputs = model(X_test_tensor)
            _, predicted = torch.max(test_outputs, 1)
            print(f'epoch {epoch }, loss {loss.item()}')
            report = classification_report(y_test, predicted.numpy(), zero_division=1)
            print(f'classification report:\n{report}')
            print('=' * 60)


main()
