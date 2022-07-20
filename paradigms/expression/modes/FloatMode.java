package expression.modes;

public class FloatMode implements Modes<Float> {
    public Float min(Float a, Float b) { return Float.min(a, b); }
    public Float max(Float a, Float b) { return Float.max(a, b); }
    public Float add(Float a, Float b) { return a + b; }
    public Float subtract(Float a, Float b) { return a - b; }
    public Float divide(Float a, Float b) { return a / b; }
    public Float multiply(Float a, Float b) { return a * b; }
    public Float count(Float a) { return (float) Integer.bitCount(Float.floatToIntBits(a)); }
    public Float abs(Float a) { return a > 0 ? a : -a; }
    public Float negate(Float a) { return -a; }
    public Float value(String s) { return Float.parseFloat(s); }
    public Float value(int a) { return (float) a; }
}
