package expression.exceptions;

public class UnexpectedCharacterException extends ParsingException {
    public UnexpectedCharacterException(String expected, String found) {
        super("expected " + expected + "; found " + found);
    }
}
