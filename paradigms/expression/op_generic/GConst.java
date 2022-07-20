package expression.op_generic;

import expression.GMSExpression;

public final class GConst<T> implements GMSExpression<T> {
    private final T value;

    public GConst(T value) { this.value = value; }

    @Override
    public T evaluate(T x) { return value; }

    @Override
    public T evaluate(T x, T y, T z) { return value; }

    @Override
    public String toString() { return value.toString(); }

    @Override
    public String toMiniString() { return value.toString(); }

    @Override
    public int priority() { return GTerm.op2priority.get(GTerm.VALUE); }

    @Override
    public boolean isAssociative() { return false; }

    @Override
    public int hashCode() { return value.hashCode(); }
}