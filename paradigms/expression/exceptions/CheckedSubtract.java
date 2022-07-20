package expression.exceptions;

import expression.MSExpression;
import expression.parser_exceptions.OverflowException;

import static expression.exceptions.CheckedTerm.SUBTRACT;

public final class CheckedSubtract extends CheckedBinaryOperation {
    public CheckedSubtract(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        if (b < 0 && max + b < a || b > 0 && min + b > a) {
            throw new OverflowException("subtract", a + " " + b);
        }
        return a - b;
    }

    @Override
    protected String symbol() {
        return "-";
    }

    @Override
    public int priority() {
        return CheckedTerm.op2priority.get(SUBTRACT);
    }

    @Override
    public boolean isAssociative() {
        return false;
    }
}
