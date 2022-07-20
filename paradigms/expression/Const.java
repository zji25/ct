package expression;

public final class Const implements MSExpression {
    private final int value;

    public Const(int value) { this.value = value; }

    public Const(String value) { this.value = Integer.parseInt(value); }

    @Override
    public int evaluate(int x) { return value; }

    @Override
    public int evaluate(int x, int y, int z) { return value; }

    @Override
    public String toString() { return Integer.toString(value); }

    @Override
    public int priority() { return Term.op2priority.get(Term.VALUE); }

    @Override
    public boolean isAssociative() { return false; }

    @Override
    public int hashCode() { return Integer.hashCode(value); }
}