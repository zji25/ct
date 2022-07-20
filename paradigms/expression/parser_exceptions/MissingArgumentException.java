package expression.parser_exceptions;

public class MissingArgumentException extends ParsingException {
    public MissingArgumentException() { super("argument missing :("); }
}
