package expression.modes;

import expression.parser_exceptions.DivisionByZeroException;

public class LongMode implements Modes<Long> {
    public Long min(Long a, Long b) { return a < b ? a : b; }
    public Long max(Long a, Long b) { return a < b ? b : a; }
    public Long add(Long a, Long b) { return a + b; }
    public Long subtract(Long a, Long b) { return a - b; }
    public Long divide(Long a, Long b) {
        if (b == 0) throw new DivisionByZeroException();
        return a / b;
    }
    public Long multiply(Long a, Long b) { return a * b; }
    public Long count(Long a) { return (long) Long.bitCount(a); }
    public Long abs(Long a) { return a > 0 ? a : -a; }
    public Long negate(Long a) { return -a; }
    public Long value(String s) { return Long.parseLong(s); }
    public Long value(int a) { return (long) a; }
}
