package expression;

public final class Max extends BinaryOperation {
    public Max(MSExpression first, MSExpression second) { super(first, second); }

    @Override
    protected int apply(int a, int b) { return a < b ? b : a; }

    @Override
    protected String symbol() { return "max"; }

    @Override
    public int priority() { return Term.op2priority.get(Term.MAXIMUM); }

    @Override
    public boolean isAssociative() { return true; }
}
