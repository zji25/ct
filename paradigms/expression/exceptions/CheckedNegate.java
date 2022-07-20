package expression.exceptions;

import expression.MSExpression;
import expression.parser_exceptions.OverflowException;

public class CheckedNegate extends CheckedUnaryOperation {
    public CheckedNegate(MSExpression first) {
        super(first);
    }

    @Override
    protected int apply(int a) {
        if (a == min) throw new OverflowException("negate", Integer.toString(a));
        return -a;
    }

    @Override
    protected String symbol() {
        return "-";
    }
}
