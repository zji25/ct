package expression;

public class Abs extends UnaryOperation {
    public Abs(MSExpression first) { super(first); }

    @Override
    protected int apply(int a) {
        if (a >= 0) return a;
        return -a;
    }

    @Override
    protected String symbol() { return "abs"; }
}
