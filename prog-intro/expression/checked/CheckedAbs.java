package expression.checked;

import expression.MSExpression;
import expression.UnaryOperation;
import expression.exceptions.OverflowException;

public class CheckedAbs extends UnaryOperation {
    public CheckedAbs(MSExpression first) {
        super(first);
    }

    @Override
    protected int apply(int a) {
        if (a == min) throw new OverflowException("abs", Integer.toString(a));
        if (a >= 0) return a;
        return -a;
    }

    @Override
    protected String symbol() {
        return "abs";
    }
}
