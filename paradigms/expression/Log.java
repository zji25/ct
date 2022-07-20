package expression;

public final class Log extends BinaryOperation {
    public Log(MSExpression first, MSExpression second) { super(first, second); }

    @Override
    protected int apply(int a, int b) { return (int) (Math.log(a) / Math.log(b)); }

    @Override
    protected String symbol() { return "//"; }

    @Override
    public int priority() { return Term.op2priority.get(Term.LOG); }

    @Override
    public boolean isAssociative() { return true; }
}
