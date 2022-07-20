package expression.exceptions;

public class MissingArgumentException extends ParsingException {
    public MissingArgumentException() {
        super("argument missing :(");
    }
}
