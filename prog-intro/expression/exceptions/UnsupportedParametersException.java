package expression.exceptions;

public class UnsupportedParametersException extends ArithmeticalException {
    public UnsupportedParametersException(String function, String message) {super(function + " doesn't support " + message);}
}
