package info.kgeorgiy.ja.yarunina.i18n;

public enum Type {
    SENTENCE("sentence"), WORD("word"), DATE("date"), NUMBER("number"), MONEY("money");
    private final String stringValue;

    Type(final String stringValue) {
        this.stringValue = stringValue;
    }

    @Override
    public String toString() {
        return stringValue;
    }

    public boolean isText() {
        return this.equals(SENTENCE) || this.equals(WORD);
    }
}
