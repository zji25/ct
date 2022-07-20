package expression.op_generic;

import expression.GMSExpression;
import expression.modes.Modes;
import java.util.Objects;

public abstract class GBinaryOperation<T> implements GMSExpression<T> {
    private final GMSExpression<T> first, second;
    protected final Modes<T> mode;

    public GBinaryOperation(GMSExpression<T> first, GMSExpression<T> second, Modes<T> mode) {
        this.first = first;
        this.second = second;
        this.mode = mode;
    }

    protected abstract T apply(T a, T b);
    protected abstract String symbol();

    @Override
    public T evaluate(T x) { return apply(first.evaluate(x), second.evaluate(x)); }

    @Override
    public T evaluate(T x, T y, T z) { return apply(first.evaluate(x, y, z), second.evaluate(x, y, z)); }

    @Override
    public String toString() { return "(" + first + " " + symbol() + " " + second + ")"; }

    @Override
    public String toMiniString() {
        StringBuilder builder = new StringBuilder();
        if (first.priority() < priority()) builder.append('(').append(first.toMiniString()).append(')');
        else builder.append(first.toMiniString());
        builder.append(' ').append(symbol()).append(' ');
        if (second.priority() < priority() || (second.priority() == priority() && (!isAssociative()
                || !second.isAssociative() && second.priority() == GTerm.op2priority.get(GTerm.DIVIDE)))) {
            builder.append('(').append(second.toMiniString()).append(')');
        } else builder.append(second.toMiniString());
        return builder.toString();
    }

    @Override
    public int hashCode() { return Objects.hash(first, second, getClass()); }
}