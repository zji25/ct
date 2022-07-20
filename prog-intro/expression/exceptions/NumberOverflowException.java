package expression.exceptions;

public class NumberOverflowException extends ExpressionException {
    public NumberOverflowException(String number) {
        super(number + " overflows int type");
    }
}
