package expression.checked;

import expression.BinaryOperation;
import expression.MSExpression;
import expression.exceptions.DivisionByZeroException;
import expression.exceptions.OverflowException;

public final class CheckedDivide extends BinaryOperation {
    public CheckedDivide(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        if (b == 0) throw new DivisionByZeroException();
        if (a == min && b == -1) {
            throw new OverflowException("divide", a + " " + b);
        }
        return a / b;
    }

    @Override
    protected String symbol() {
        return "/";
    }

    @Override
    public int priority() {
        return 1;
    }

    @Override
    public boolean isAssociative() {
        return false;
    }
}
