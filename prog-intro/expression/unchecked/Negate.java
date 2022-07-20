package expression.unchecked;

import expression.MSExpression;
import expression.UnaryOperation;

public class Negate extends UnaryOperation {
    public Negate(MSExpression first) {
        super(first);
    }

    @Override
    protected int apply(int a) {
        return -a;
    }

    @Override
    protected String symbol() {
        return "-";
    }
}
