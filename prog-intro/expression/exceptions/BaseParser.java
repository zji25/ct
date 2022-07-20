package expression.exceptions;

public class BaseParser {
    protected static final char END = 0;
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
            //
        }
        return pos == expected.length();
    }

    protected void expect(char expected) {
        if (expected != current) {
            throw new UnexpectedCharacterException(String.format("\"%c\"", expected),
                    String.format("\"%c\"", current));
        }
        take();
    }

    protected void inspect(char var1, char var2) {
        if (var1 != current && var2 != current) {
            throw new UnexpectedCharacterException(String.format("\"%c\" or \"%c\"", var1, var2),
                    String.format("\"%c\"", current));
        }
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
        if (negative) {
            builder.append('-');
        }
        while (Character.isDigit(current)) {
            builder.append(current);
            take();
        }
        if (Character.isLetter(current)) {
            throw new UnexpectedCharacterException("whitespace or operand", "letter \"" + current + "\"");
        }
        try {
            return Integer.parseInt(builder.toString());
        } catch (NumberFormatException e) {
            throw new NumberOverflowException(builder.toString());
        }
    }
}