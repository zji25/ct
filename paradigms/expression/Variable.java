package expression;

import expression.parser_exceptions.UnexpectedCharactersException;

public final class Variable implements MSExpression {
    private final String var;

    public Variable(String var) { this.var = var; }

    @Override
    public int evaluate(int x) { return x; }

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
    public String toString() { return var; }

    @Override
    public String toMiniString() { return var; }

    @Override
    public int priority() { return Term.op2priority.get(Term.VALUE); }

    @Override
    public boolean isAssociative() { return false; }

    @Override
    public int hashCode() { return var.hashCode(); }
}