package expression.unchecked;

import expression.BinaryOperation;
import expression.MSExpression;
import expression.Term;

import static expression.Term.ADD;

public final class Add extends BinaryOperation {
    public Add(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        return a + b;
    }

    @Override
    protected String symbol() {
        return "+";
    }

    @Override
    public int priority() {
        return Term.op2priority.get(ADD);
    }

    @Override
    public boolean isAssociative() {
        return true;
    }
}