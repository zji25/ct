package expression.op_generic;

import expression.GMSExpression;
import expression.modes.Modes;

public class GNegate<T> extends GUnaryOperation<T> {
    public GNegate(GMSExpression<T> first, Modes<T> mode) { super(first, mode); }

    @Override
    protected T apply(T a) { return mode.negate(a); }

    @Override
    protected String symbol() { return "-"; }
}
