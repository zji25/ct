package expression.exceptions;

import expression.MSExpression;
import expression.op_generic.GTerm;
import expression.parser_exceptions.OverflowException;
import expression.parser_exceptions.UnsupportedParametersException;

import static expression.exceptions.CheckedTerm.POW;

public final class CheckedPow extends CheckedBinaryOperation {
    public CheckedPow(MSExpression first, MSExpression second) {
        super(first, second);
    }

    @Override
    protected int apply(int a, int b) {
        if (a == 0 && b == 0) throw new UnsupportedParametersException("power", "0**0");
        if (b < 0) throw new UnsupportedParametersException("power", "negative power: " + b);
        int result = 1;
        while (b > 0) {
            if ((b & 1) == 0) {
                if (a != 0 && a * a / a != a) throw new OverflowException("power", a + " " + b);
                a *= a;
                b >>>= 1;
            } else {
                if (result != 0 && a * result / result != a) throw new OverflowException("power", a + " " + b);
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
        return CheckedTerm.op2priority.get(CheckedTerm.POW);
    }

    @Override
    public boolean isAssociative() {
        return false;
    }
}
