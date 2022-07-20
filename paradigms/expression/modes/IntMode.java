package expression.modes;

import expression.parser_exceptions.DivisionByZeroException;
import expression.parser_exceptions.OverflowException;

public class IntMode implements Modes<Integer> {
    private final boolean checked;
    private final int max = Integer.MAX_VALUE;
    private final int min = Integer.MIN_VALUE;

    public IntMode(boolean checked) { this.checked = checked; }

    public Integer min(Integer a, Integer b) {
        if (a <= b) return a;
        return b;
    }
    public Integer max(Integer a, Integer b) {
        if (a <= b) return b;
        return a;
    }
    public Integer add(Integer a, Integer b) {
        if (checked && (b > 0 && max - b < a || b < 0 && min - b > a)) {
            throw new OverflowException("add", a + " " + b);
        }
        return a + b;
    }
    public Integer subtract(Integer a, Integer b) {
        if (checked && (b < 0 && max + b < a || b > 0 && min + b > a)) {
            throw new OverflowException("subtract", a + " " + b);
        }
        return a - b;
    }
    public Integer multiply(Integer a, Integer b) {
        if (checked && (b != 0 && a * b / b != a || a != 0 && a * b / a != b)) {
            throw new OverflowException("multiply", a + " " + b);
        }
        return a * b;
    }
    public Integer divide(Integer a, Integer b) {
        if (b == 0) throw new DivisionByZeroException();
        if (checked && a == min && b == -1) {
            throw new OverflowException("divide", a + " " + b);
        }
        return a / b;
    }
    public Integer count(Integer a) { return Integer.bitCount(a); }
    public Integer abs(Integer a) {
        if (checked && a == min) throw new OverflowException("abs", Integer.toString(a));
        if (a >= 0) return a;
        return -a;
    }
    public Integer negate(Integer a) {
        if (checked && a == min) throw new OverflowException("negate", Integer.toString(a));
        return -a;
    }
    public Integer value(String s) { return Integer.parseInt(s); }
    public Integer value(int a) { return a; }
}
