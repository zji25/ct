package expression.exceptions;

import expression.MSExpression;
import expression.parser_exceptions.DivisionByZeroException;
import expression.parser_exceptions.OverflowException;

public final class CheckedDivide extends CheckedBinaryOperation {
    public CheckedDivide(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        if (b == 0) throw new DivisionByZeroException();
        if (a == min && b == -1) throw new OverflowException("divide", a + " " + b);
        return a / b;
    }

    @Override
    protected String symbol() {
        return "/";
    }

    @Override
    public int priority() {
        return CheckedTerm.op2priority.get(CheckedTerm.DIVIDE);
    }

    @Override
    public boolean isAssociative() {
        return false;
    }
}
