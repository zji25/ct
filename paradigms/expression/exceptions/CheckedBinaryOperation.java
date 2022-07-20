package expression.exceptions;

import expression.MSExpression;

import java.util.Objects;

public abstract class CheckedBinaryOperation implements MSExpression {
    private final MSExpression first, second;
    protected final int max = Integer.MAX_VALUE;
    protected final int min = Integer.MIN_VALUE;

    public CheckedBinaryOperation(MSExpression first, MSExpression second) {
        this.first = first;
        this.second = second;
    }

    protected abstract int apply(int a, int b);
    protected abstract String symbol();

    @Override
    public int evaluate(int x) {
        return apply(first.evaluate(x), second.evaluate(x));
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return apply(first.evaluate(x, y, z), second.evaluate(x, y, z));
    }

    @Override
    public String toString() {
        return "(" + first + " " + symbol() + " " + second + ")";
    }

    @Override
    public String toMiniString() {
        StringBuilder builder = new StringBuilder();
        if (first.priority() < priority()) {
            builder.append('(').append(first.toMiniString()).append(')');
        } else {
            builder.append(first.toMiniString());
        }
        builder.append(' ').append(symbol()).append(' ');
        if (second.priority() < priority()
                || (second.priority() == priority()
                && (!isAssociative()
                || !second.isAssociative() && second.priority() == CheckedTerm.op2priority.get(CheckedTerm.DIVIDE)))
                || second.priority() == priority() && priority() == -1 && second.getClass() != getClass()) {
            builder.append('(').append(second.toMiniString()).append(')');
        } else {
            builder.append(second.toMiniString());
        }
        return builder.toString();
    }

    @Override
    public int hashCode() {
        return Objects.hash(first, second, getClass());
    }
}