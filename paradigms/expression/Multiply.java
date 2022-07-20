package expression;

public final class Multiply extends BinaryOperation {
    public Multiply(MSExpression first, MSExpression second) { super(first, second); }

    @Override
    protected int apply(int a, int b) { return a * b; }

    @Override
    protected String symbol() { return "*"; }

    @Override
    public int priority() { return Term.op2priority.get(Term.MULTIPLY); }

    @Override
    public boolean isAssociative() { return true; }
}