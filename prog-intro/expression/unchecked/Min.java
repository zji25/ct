package expression.unchecked;

import expression.BinaryOperation;
import expression.MSExpression;
import expression.Term;

import static expression.Term.MINIMUM;

public final class Min extends BinaryOperation {
    public Min(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        if (a <= b) return a;
        return b;
    }

    @Override
    protected String symbol() {
        return "min";
    }

    @Override
    public int priority() {
        return Term.op2priority.get(MINIMUM);
    }

    @Override
    public boolean isAssociative() {
        return true;
    }
}
