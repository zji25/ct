package expression.unchecked;

import expression.MSExpression;
import expression.UnaryOperation;

public class TZeroes extends UnaryOperation {
    public TZeroes(MSExpression first) {
        super(first);
    }

    @Override
    protected int apply(int a) {
        String bin = Integer.toBinaryString(a);
        int result = 0;
        int i = bin.length() - 1;
        while (i >= 0) {
            if (bin.charAt(i) == '0') result++;
            else break;
            i--;
        }
        return a != 0 ? result : 32;
    }

    @Override
    protected String symbol() {
        return "t0";
    }
}
