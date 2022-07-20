package expression;

public interface GMSExpression<T> {
    T evaluate(T x);
    T evaluate(T x, T y, T z);
    int priority();
    boolean isAssociative();
    String toMiniString();
}
