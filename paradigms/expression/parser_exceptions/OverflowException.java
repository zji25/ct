package expression.parser_exceptions;

public class OverflowException extends ArithmeticalException {
    public OverflowException(String operation, String arguments) { super(operation + " overflow: " + arguments); }
}
