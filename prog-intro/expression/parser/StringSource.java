package expression.parser;

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
    public IllegalArgumentException error(final String message) {
        return new IllegalArgumentException(String.format(
                "%s at %d (following input : %s)",
                message, position, expression.substring(
                        position, Math.min(expression.length(), position + 100
                        ))));
    }
}

