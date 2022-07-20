package expression.parser;

import expression.GMSExpression;
import expression.parser_exceptions.ParsingException;

public interface GenericParser<T> {
    GMSExpression<T> parse(String expression) throws ParsingException;
}
