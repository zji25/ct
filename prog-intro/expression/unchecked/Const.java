package expression.unchecked;

import expression.MSExpression;
import expression.Term;

import static expression.Term.VALUE;

public final class Const implements MSExpression {
    private final int value;

    public Const(int value) {
        this.value = value;
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
        return Term.op2priority.get(VALUE);
    }

    @Override
    public boolean isAssociative() {
        return false;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Const) return value == ((Const) obj).value;
        return false;
    }

    @Override
    public int hashCode() {
        return Integer.hashCode(value);
    }
}