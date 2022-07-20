package expression.exceptions;

import expression.MSExpression;

public final class CheckedMin extends CheckedBinaryOperation {
    public CheckedMin(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        return a < b ? a : b;
    }

    @Override
    protected String symbol() {
        return "min";
    }

    @Override
    public int priority() {
        return CheckedTerm.op2priority.get(CheckedTerm.MINIMUM);
    }

    @Override
    public boolean isAssociative() {
        return true;
    }
}