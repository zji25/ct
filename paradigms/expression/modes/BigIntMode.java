package expression.modes;

import expression.parser_exceptions.DivisionByZeroException;
import java.math.BigInteger;

public class BigIntMode implements Modes<BigInteger> {
    public BigInteger min(BigInteger a, BigInteger b) { return a.min(b); }
    public BigInteger max(BigInteger a, BigInteger b) { return a.max(b); }
    public BigInteger add(BigInteger a, BigInteger b) { return a.add(b); }
    public BigInteger subtract(BigInteger a, BigInteger b) { return a.subtract(b); }
    public BigInteger multiply(BigInteger a, BigInteger b) { return a.multiply(b); }
    public BigInteger divide(BigInteger a, BigInteger b) {
        if (b.equals(BigInteger.valueOf(0))) throw new DivisionByZeroException();
        return a.divide(b);
    }
    public BigInteger count(BigInteger a) { return BigInteger.valueOf(a.bitCount()); }
    public BigInteger abs(BigInteger a) { return a.abs(); }
    public BigInteger negate(BigInteger a) { return a.negate(); }
    public BigInteger value(String s) { return new BigInteger(s); }
    public BigInteger value(int a) { return BigInteger.valueOf(a); }
}

