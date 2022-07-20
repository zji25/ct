package expression.unchecked;

import expression.BinaryOperation;
import expression.MSExpression;
import expression.Term;

import static expression.Term.MAXIMUM;

public final class Max extends BinaryOperation {
    public Max(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        if (a < b) return b;
        return a;
    }

    @Override
    protected String symbol() {
        return "max";
    }

    @Override
    public int priority() {
        return Term.op2priority.get(MAXIMUM);
    }

    @Override
    public boolean isAssociative() {
        return true;
    }
}
