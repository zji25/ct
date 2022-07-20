package expression.checked;

import expression.BinaryOperation;
import expression.MSExpression;
import expression.Term;
import expression.exceptions.LogarithmException;

import static expression.Term.LOG;

public final class CheckedLog extends BinaryOperation {
    public CheckedLog(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        if (a <= 0 || b <= 0) {
            throw new LogarithmException("negative argument");
        } else if (b == 1) {
            throw new LogarithmException("base can't be equal to 1");
        }
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
        return false;
    }
}
