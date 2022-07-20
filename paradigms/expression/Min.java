package expression;

public final class Min extends BinaryOperation {
    public Min(MSExpression first, MSExpression second) { super(first, second); }

    @Override
    protected int apply(int a, int b) { return a < b ? a : b; }

    @Override
    protected String symbol() { return "min"; }

    @Override
    public int priority() { return Term.op2priority.get(Term.MINIMUM); }

    @Override
    public boolean isAssociative() { return true; }
}
