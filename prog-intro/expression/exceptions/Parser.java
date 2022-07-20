package expression.exceptions;

import expression.TripleExpression;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FunctionalInterface
public interface Parser {
    TripleExpression parse(String expression) throws ExpressionException;
}