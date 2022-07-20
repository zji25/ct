package expression;

public class TZeroes extends UnaryOperation {
    public TZeroes(MSExpression first) { super(first); }

    @Override
    protected int apply(int a) {
        String bin = Integer.toBinaryString(a);
        int result = 0, i = bin.length() - 1;
        while (i >= 0) {
            if (bin.charAt(i) == '0') result++;
            else break;
            i--;
        }
        return a != 0 ? result : 32;
    }

    @Override
    protected String symbol() { return "t0"; }
}
