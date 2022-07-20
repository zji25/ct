package expression.op_generic;

import java.util.Map;

public enum GTerm {
    MINIMUM, MAXIMUM, ADD, SUBTRACT, MULTIPLY, DIVIDE, UNARY, VALUE;

    public static final int MIN_PRIORITY = -1;
    public static final int MAX_PRIORITY = 2;
    public static final Map<GTerm, Integer> op2priority = Map.of(
            MINIMUM, -1, MAXIMUM, -1,
            ADD, 0, SUBTRACT, 0,
            MULTIPLY, 1, DIVIDE, 1,
            UNARY, 2, VALUE, 2);
    public static final Map<Character, Integer> symbol2priority = Map.of(
            'm', -1,
            '+', 0, '-', 0,
            '*', 1, '/', 1);
}
