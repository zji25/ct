package expression.exceptions;

public class ExpressionException extends RuntimeException {
    public ExpressionException(String message) {
        super(message);
    }
    public ExpressionException() {
        super();
    }
}
