package expression.parser;

import expression.parser_exceptions.UnexpectedCharactersException;

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
        if (current != expected) return false;
        take();
        return true;
    }

    protected boolean test(String expected) {
        int i = 0;
        while (i < expected.length() && test(expected.charAt(i))) i++;
        if (i == expected.length()) return true;
        source.rollback(i + 1);
        current = source.next();
        return false;
    }

    protected void expect(char expected) {
        if (expected != current) throw new UnexpectedCharactersException(String.format("\"%c\"", expected), String.format("\"%c\"", current));
        take();
    }

    protected void skip() { while (Character.isWhitespace(current)) take(); }

    protected boolean between(char left, char right) { return left <= current && current <= right; }

    protected String nextNumber(boolean negative) {
        StringBuilder builder = new StringBuilder();
        if (negative) builder.append('-');
        while (Character.isDigit(current)) {
            builder.append(current);
            take();
        }
        if (Character.isLetter(current)) throw new UnexpectedCharactersException("whitespace or operand", "\"" + current + "\"");
        return builder.toString();
    }
}
