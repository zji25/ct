package expression.op_generic;

import expression.GMSExpression;
import expression.parser_exceptions.UnexpectedCharactersException;
import java.util.Objects;

import static expression.op_generic.GTerm.VALUE;

public final class GVariable<T> implements GMSExpression<T> {
    private final String var;

    public GVariable(String var) { this.var = var; }

    @Override
    public T evaluate(T x) { return x; }

    @Override
    public T evaluate(T x, T y, T z) {
        return switch (var) {
            case "x" -> x;
            case "y" -> y;
            case "z" -> z;
            default -> throw new UnexpectedCharactersException("x, y or z", var);
        };
    }

    @Override
    public String toString() { return var; }

    @Override
    public String toMiniString() { return var; }

    @Override
    public int priority() { return GTerm.op2priority.get(VALUE); }

    @Override
    public boolean isAssociative() { return false; }

    @Override
    public int hashCode() { return Objects.hashCode(var); }
}