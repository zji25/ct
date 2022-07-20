package expression.unchecked;

import expression.BinaryOperation;
import expression.MSExpression;
import expression.Term;

import static expression.Term.POW;

public final class Pow extends BinaryOperation {
    public Pow(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        return (int) Math.pow(a, b);
    }

    @Override
    protected String symbol() {
        return "**";
    }

    @Override
    public int priority() {
        return Term.op2priority.get(POW);
    }

    @Override
    public boolean isAssociative() {
        return true;
    }
}
