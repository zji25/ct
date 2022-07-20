package expression.parser;

public class StringSource implements CharSource {
    private final String expression;
    private int position;

    public StringSource(String expression) { this.expression = expression; }

    @Override
    public char next() { return expression.charAt(position++); }

    @Override
    public boolean hasNext() { return position < expression.length(); }

    @Override
    public void rollback(int amount) { position = Math.max(position - amount, 0); }
}

