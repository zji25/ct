package expression.exceptions;

import expression.MSExpression;
import expression.parser_exceptions.OverflowException;

import static expression.exceptions.CheckedTerm.MULTIPLY;

public final class CheckedMultiply extends CheckedBinaryOperation {
    public CheckedMultiply(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        if (b != 0 && a * b / b != a || a != 0 && a * b / a != b) {
            throw new OverflowException("multiply", a + " " + b);
        }
        return a * b;
    }

    @Override
    protected String symbol() {
        return "*";
    }

    @Override
    public int priority() {
        return CheckedTerm.op2priority.get(MULTIPLY);
    }

    @Override
    public boolean isAssociative() {
        return true;
    }
}