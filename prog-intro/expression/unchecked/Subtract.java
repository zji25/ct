package expression.unchecked;

import expression.BinaryOperation;
import expression.MSExpression;
import expression.Term;

import static expression.Term.SUBTRACT;

public final class Subtract extends BinaryOperation {
    public Subtract(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        return a - b;
    }

    @Override
    protected String symbol() {
        return "-";
    }

    @Override
    public int priority() {
        return Term.op2priority.get(SUBTRACT);
    }

    @Override
    public boolean isAssociative() {
        return false;
    }
}