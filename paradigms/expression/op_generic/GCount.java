package expression.op_generic;

import expression.GMSExpression;
import expression.modes.Modes;

public class GCount<T> extends GUnaryOperation<T> {
    public GCount(GMSExpression<T> first, Modes<T> mode) { super(first, mode); }

    @Override
    protected T apply(T a) { return mode.count(a); }

    @Override
    protected String symbol() { return "count"; }
}
