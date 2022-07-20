package expression.parser_exceptions;

public class DivisionByZeroException extends ArithmeticalException {
    public DivisionByZeroException() { super("division by zero"); }
}
