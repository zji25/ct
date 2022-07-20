package expression.parser_exceptions;

public class UnexpectedCharactersException extends ParsingException {
    public UnexpectedCharactersException(String expected, String found) {
        super("expected " + expected + "; found " + found);
    }
}
