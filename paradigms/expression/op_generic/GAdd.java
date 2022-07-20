package expression.op_generic;

import expression.GMSExpression;
import expression.modes.Modes;

public final class GAdd<T> extends GBinaryOperation<T> {
    public GAdd(GMSExpression<T> first, GMSExpression<T> second, Modes<T> mode) { super(first, second, mode); }

    @Override
    protected T apply(T a, T b) { return mode.add(a, b); }

    @Override
    protected String symbol() { return "+"; }

    @Override
    public int priority() { return GTerm.op2priority.get(GTerm.ADD); }

    @Override
    public boolean isAssociative() { return true; }
}