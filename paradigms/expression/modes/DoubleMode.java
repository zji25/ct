package expression.modes;

public class DoubleMode implements Modes<Double> {
    public Double min(Double a, Double b) { return Double.min(a, b); }
    public Double max(Double a, Double b) { return Double.max(a, b); }
    public Double add(Double a, Double b) { return a + b; }
    public Double subtract(Double a, Double b) { return a - b; }
    public Double multiply(Double a, Double b) { return a * b; }
    public Double divide(Double a, Double b) { return a / b; }
    public Double count(Double a) { return (double) Long.bitCount(Double.doubleToLongBits(a)); }
    public Double abs(Double a) { return a >= 0 ? a : -a; }
    public Double negate(Double a) { return -a; }
    public Double value(String s) { return Double.parseDouble(s); }
    public Double value(int a) { return (double) a; }
}

