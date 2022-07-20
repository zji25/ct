package expression.op_generic;

import expression.GMSExpression;
import expression.modes.Modes;

public final class GMax<T> extends GBinaryOperation<T> {
    public GMax(GMSExpression<T> first, GMSExpression<T> second, Modes<T> mode) {
        super(first, second, mode);
    }

    @Override
    protected T apply(T a, T b) { return mode.max(a, b); }

    @Override
    protected String symbol() { return "max"; }

    @Override
    public int priority() { return GTerm.op2priority.get(GTerm.MAXIMUM); }

    @Override
    public boolean isAssociative() { return true; }
}
