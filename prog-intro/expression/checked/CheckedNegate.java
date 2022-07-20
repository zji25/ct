package expression.checked;

import expression.MSExpression;
import expression.UnaryOperation;
import expression.exceptions.OverflowException;

public class CheckedNegate extends UnaryOperation {
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
