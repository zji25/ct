package expression.exceptions;

import expression.MSExpression;
import expression.parser_exceptions.UnsupportedParametersException;

public final class CheckedLog extends CheckedBinaryOperation {
    public CheckedLog(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        if (a <= 0 || b <= 0) throw new UnsupportedParametersException("log", a + " " + b);
        else if (b == 1) throw new UnsupportedParametersException("log", a + " " + b);
        return (int) (Math.log(a) / Math.log(b));
    }

    @Override
    protected String symbol() {
        return "//";
    }

    @Override
    public int priority() {
        return CheckedTerm.op2priority.get(CheckedTerm.LOG);
    }

    @Override
    public boolean isAssociative() {
        return false;
    }
}
