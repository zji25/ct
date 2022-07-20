package expression.op_generic;

import expression.GMSExpression;
import expression.modes.Modes;

public final class GMin<T> extends GBinaryOperation<T> {
    public GMin(GMSExpression<T> first, GMSExpression<T> second, Modes<T> mode) { super(first, second, mode); }

    @Override
    protected T apply(T a, T b) { return mode.min(a, b); }

    @Override
    protected String symbol() { return "min"; }

    @Override
    public int priority() { return GTerm.op2priority.get(GTerm.MINIMUM); }

    @Override
    public boolean isAssociative() { return true; }
}
