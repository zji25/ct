package expression.exceptions;

import expression.MSExpression;
import expression.parser_exceptions.OverflowException;

public class CheckedAbs extends CheckedUnaryOperation {
    public CheckedAbs(MSExpression first) {
        super(first);
    }

    @Override
    protected int apply(int a) {
        if (a == min) throw new OverflowException("abs", Integer.toString(a));
        return a > 0 ? a : -a;
    }

    @Override
    protected String symbol() {
        return "abs";
    }
}
