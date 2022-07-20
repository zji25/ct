package expression.unchecked;

import expression.MSExpression;
import expression.Term;
import expression.exceptions.UnexpectedCharacterException;

import java.util.Objects;

import static expression.Term.VALUE;

public final class Variable implements MSExpression {
    private final String var;

    public Variable(String var) {
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
            default -> throw new UnexpectedCharacterException("x, y or z", var);
        };
    }

    @Override
    public String toString() {
        return var;
    }

    @Override
    public int priority() {
        return Term.op2priority.get(VALUE);
    }

    @Override
    public boolean isAssociative() {
        return false;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Variable) return Objects.equals(var, ((Variable) obj).var);
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(var);
    }
}