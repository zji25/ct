package expression;

public class LZeroes extends UnaryOperation {
    public LZeroes(MSExpression first) { super(first); }

    @Override
    protected int apply(int a) { return a != 0 ? 32 - Integer.toBinaryString(a).length() : 32; }

    @Override
    protected String symbol() { return "l0"; }
}

