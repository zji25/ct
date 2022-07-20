package expression;

import java.util.Objects;

import static expression.Term.DIVIDE;

public abstract class BinaryOperation implements MSExpression {
    private final MSExpression first, second;
    protected final int max = Integer.MAX_VALUE;
    protected final int min = Integer.MIN_VALUE;

    public BinaryOperation(MSExpression first, MSExpression second) {
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
                || !second.isAssociative() && second.priority() == Term.op2priority.get(DIVIDE)))) {
            builder.append('(').append(second.toMiniString()).append(')');
        } else {
            builder.append(second.toMiniString());
        }
        return builder.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof BinaryOperation temp) {
            return Objects.equals(getClass(), obj.getClass())
                    && Objects.equals(first, temp.first)
                    && Objects.equals(second, temp.second);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(first, second, getClass());
    }
}