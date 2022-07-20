package expression.exceptions;

import expression.MSExpression;
import expression.parser_exceptions.UnexpectedCharactersException;

import java.util.Objects;


public final class CheckedVariable implements MSExpression {
    private final String var;

    public CheckedVariable(String var) {
        this.var = var;
    }

    @Override
    public int evaluate(int x) {
        return x;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return switch (var) {
            case "x" -> x;
            case "y" -> y;
            case "z" -> z;
            default -> throw new UnexpectedCharactersException("x, y or z", var);
        };
    }

    @Override
    public String toString() {
        return var;
    }

    @Override
    public int priority() {
        return CheckedTerm.op2priority.get(CheckedTerm.VALUE);
    }

    @Override
    public boolean isAssociative() {
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(var);
    }
}