package expression.op_generic;

import expression.GMSExpression;
import expression.modes.Modes;

public class GAbs<T> extends GUnaryOperation<T> {
    public GAbs(GMSExpression<T> first, Modes<T> mode) { super(first, mode); }

    @Override
    protected T apply(T a) { return mode.abs(a); }

    @Override
    protected String symbol() { return "abs"; }
}
