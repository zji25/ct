package expression.checked;

import expression.BinaryOperation;
import expression.MSExpression;
import expression.Term;
import expression.exceptions.OverflowException;
import expression.exceptions.UnsupportedParametersException;

import static expression.Term.POW;

public final class CheckedPow extends BinaryOperation {
    public CheckedPow(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        if (a == 0 && b == 0) {
            throw new UnsupportedParametersException("pow", "0**0");
        }
        if (b < 0) {
            throw new UnsupportedParametersException("pow", "negative power: " + b);
        }
        int result = 1;
        while (b > 0) {
            if ((b & 1) == 0) {
                if (a != 0 && a * a / a != a) {
                    throw new OverflowException("power", a + " " + b);
                }
                a *= a;
                b >>>= 1;
            } else {
                if (result != 0 && a * result / result != a) {
                    throw new OverflowException("power", a + " " + b);
                }
                result *= a;
                b--;
            }
        }
        return result;
    }

    @Override
    protected String symbol() {
        return "**";
    }

    @Override
    public int priority() {
        return Term.op2priority.get(POW);
    }

    @Override
    public boolean isAssociative() {
        return false;
    }
}
