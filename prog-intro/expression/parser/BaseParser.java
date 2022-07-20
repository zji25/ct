package expression.parser;

public class BaseParser {
    public static final char END = 0;
    protected StringSource source;
    protected char current;

    protected char take() {
        char result = current;
        current = source.hasNext() ? source.next() : END;
        return result;
    }

    protected boolean test(char expected) {
        if (current == expected) {
            take();
            return true;
        }
        return false;
    }

    protected boolean test(String expected) {
        int pos = 0;
        while (pos < expected.length() && test(expected.charAt(pos++))){

        }
        return pos == expected.length();
    }

    protected void expect(char expected) {
        if (expected != current) {
            throw error(String.format("expected \"%c\", found \"%c\"", expected, current));
        }
        take();
    }

    protected void skip() {
        while (Character.isWhitespace(current)) {
            take();
        }
    }

    protected boolean between(Character left, Character right) {
        return left <= current && current <= right;
    }

    protected int nextInt(boolean negative) {
        StringBuilder builder = new StringBuilder();
        if (negative) builder.append('-');
        while (Character.isDigit(current)) {
            builder.append(current);
            take();
        }
        try {
            return Integer.parseInt(builder.toString());
        } catch (NumberFormatException e) {
            throw error("invalid number");
        }
    }

    protected IllegalArgumentException error(String message) {
        throw source.error(message);
    }
}
