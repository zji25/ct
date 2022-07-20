package expression.exceptions;

import expression.MSExpression;

public final class CheckedConst implements MSExpression {
    private final int value;

    public CheckedConst(String value) {
        this.value = Integer.parseInt(value);
    }

    @Override
    public int evaluate(int x) {
        return value;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return value;
    }

    @Override
    public String toString() {
        return Integer.toString(value);
    }

    @Override
    public int priority() {
        return CheckedTerm.op2priority.get(CheckedTerm.VALUE);
    }

    @Override
    public boolean isAssociative() {
        return false;
    }

    @Override
    public int hashCode() {
        return Integer.hashCode(value);
    }
}