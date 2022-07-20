package expression;

import java.util.Map;

public enum Term {
    MINIMUM, MAXIMUM, ADD, SUBTRACT, MULTIPLY, DIVIDE, POW, LOG, UNARY, VALUE;

    public static final int MIN_PRIORITY = -1;
    public static final int DOUBLES_PRIORITY = 2;
    public static final int MAX_PRIORITY = 3;
    public static final Map<Term, Integer> op2priority = Map.of(
            MINIMUM, -1, MAXIMUM, -1,
            ADD, 0, SUBTRACT, 0,
            MULTIPLY, 1, DIVIDE, 1,
            POW, 2, LOG, 2,
            UNARY, 3, VALUE, 3);
    public static final Map<Character, Integer> symbol2priority = Map.of(
            'm', -1,
            '+', 0, '-', 0,
            '*', 1, '/', 1);
}

