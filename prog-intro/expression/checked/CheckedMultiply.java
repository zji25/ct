package expression.checked;

import expression.BinaryOperation;
import expression.MSExpression;
import expression.exceptions.OverflowException;

public final class CheckedMultiply extends BinaryOperation {
    public CheckedMultiply(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        if (b != 0 && a * b / b != a || a != 0 && a * b / a != b) {
            throw new OverflowException("multiply", a + " " + b);
        }
        return a * b;
    }

    @Override
    protected String symbol() {
        return "*";
    }

    @Override
    public int priority() {
        return 1;
    }

    @Override
    public boolean isAssociative() {
        return true;
    }
}