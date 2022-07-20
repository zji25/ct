package expression.exceptions;

public class StringSource implements CharSource {
    private final String expression;
    private int position;

    public StringSource(String expression) {
        this.expression = expression;
    }

    @Override
    public char next() {
        return expression.charAt(position++);
    }

    @Override
    public boolean hasNext() {
        return position < expression.length();
    }

    @Override
    public boolean checkNext(char c) {
        return position < expression.length() && expression.charAt(position) == c;
    }
}