package expression.exceptions;

public class OverflowException extends ExpressionException {
    public OverflowException(String operation, String arguments) {
        super(operation + " overflow: " + arguments);
    }
}
