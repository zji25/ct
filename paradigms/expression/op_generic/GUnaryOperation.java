package expression.op_generic;

import expression.GMSExpression;
import expression.modes.Modes;
import java.util.Objects;

public abstract class GUnaryOperation<T> implements GMSExpression<T> {
    private final GMSExpression<T> first;
    protected final Modes<T> mode;
    protected final int max = Integer.MAX_VALUE;
    protected final int min = Integer.MIN_VALUE;

    public GUnaryOperation(GMSExpression<T> first, Modes<T> mode) {
        this.first = first;
        this.mode = mode;
    }

    protected abstract T apply(T a);
    protected abstract String symbol();

    @Override
    public T evaluate(T x) { return apply(first.evaluate(x)); }

    @Override
    public T evaluate(T x, T y, T z) { return apply(first.evaluate(x, y, z)); }

    @Override
    public String toString() { return symbol() + "(" + first + ")"; }

    @Override
    public String toMiniString() {
        if (priority() == first.priority()) return symbol() + " " + first.toMiniString();
        return symbol() + "(" + first.toMiniString() + ")";
    }

    @Override
    public int priority() { return GTerm.op2priority.get(GTerm.UNARY); }

    @Override
    public boolean isAssociative() { return false; }

    @Override
    public int hashCode() { return Objects.hash(first, this.getClass()); }
}
