package expression.unchecked;

import expression.BinaryOperation;
import expression.MSExpression;
import expression.Term;

import static expression.Term.LOG;

public final class Log extends BinaryOperation {
    public Log(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        return (int) (Math.log(a) / Math.log(b));
    }

    @Override
    protected String symbol() {
        return "//";
    }

    @Override
    public int priority() {
        return Term.op2priority.get(LOG);
    }

    @Override
    public boolean isAssociative() {
        return true;
    }
}
