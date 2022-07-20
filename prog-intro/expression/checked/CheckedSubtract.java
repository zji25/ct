package expression.checked;

import expression.BinaryOperation;
import expression.MSExpression;
import expression.exceptions.OverflowException;

public final class CheckedSubtract extends BinaryOperation {
    public CheckedSubtract(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        if (b < 0 && max + b < a || b > 0 && min + b > a) {
            throw new OverflowException("subtract", a + " " + b);
        }
        return a - b;
    }

    @Override
    protected String symbol() {
        return "-";
    }

    @Override
    public int priority() {
        return 0;
    }

    @Override
    public boolean isAssociative() {
        return false;
    }
}
