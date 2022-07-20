package expression.modes;

public interface Modes<T> {
    T min(T a, T b);
    T max(T a, T b);
    T add(T a, T b);
    T subtract(T a, T b);
    T divide(T a, T b);
    T multiply(T a, T b);
    T count(T a);
    T abs(T a);
    T negate(T a);
    T value(String s);
    T value(int a);
}