package expression.exceptions;

import expression.MSExpression;

public final class CheckedMax extends CheckedBinaryOperation {
    public CheckedMax(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        return a > b ? a : b;
    }

    @Override
    protected String symbol() {
        return "max";
    }

    @Override
    public int priority() {
        return CheckedTerm.op2priority.get(CheckedTerm.MAXIMUM);
    }

    @Override
    public boolean isAssociative() {
        return true;
    }
}