package expression;

import java.util.Objects;

import static expression.Term.UNARY;

public abstract class UnaryOperation implements MSExpression {
    private final MSExpression first;
    protected final int max = Integer.MAX_VALUE;
    protected final int min = Integer.MIN_VALUE;

    public UnaryOperation(MSExpression first) {
        this.first = first;
    }

    protected abstract int apply(int a);

    protected abstract String symbol();

    @Override
    public int evaluate(int x) {
        return apply(first.evaluate(x));
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return apply(first.evaluate(x, y, z));
    }

    @Override
    public String toString() {
        return symbol() + "(" + first + ")";
    }

    @Override
    public String toMiniString() {
        if (priority() == first.priority()) {
            return symbol() + " " + first.toMiniString();
        }
        return symbol() + "(" + first.toMiniString() + ")";
    }

    @Override
    public int priority() {
        return Term.op2priority.get(UNARY);
    }

    @Override
    public boolean isAssociative() {
        return false;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof UnaryOperation temp) {
            return Objects.equals(getClass(), obj.getClass())
                    && Objects.equals(first, temp.first);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(first, this.getClass());
    }
}
