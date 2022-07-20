package expression.generic;

import expression.GMSExpression;
import expression.parser_exceptions.ArithmeticalException;
import expression.parser_exceptions.UnsupportedParametersException;
import expression.modes.*;
import expression.parser.GenericExpressionParser;

import java.util.Map;

public class GenericTabulator implements Tabulator {
    private static final Map<String, Modes<?>> SUPPORTED_MODES = Map.of(
            "i", new IntMode(true),
            "u", new IntMode(false),
            "d", new DoubleMode(),
            "bi", new BigIntMode(),
            "l", new LongMode(),
            "f", new FloatMode()
    );

    @Override
    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) {
        if (!SUPPORTED_MODES.containsKey(mode)) throw new UnsupportedParametersException("tabulate", mode);
        return calculate(SUPPORTED_MODES.get(mode), expression, x1, x2, y1, y2, z1, z2);
    }

    private <T> Object[][][] calculate(Modes<T> mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) {
        GenericExpressionParser<T> parser = new GenericExpressionParser<>(mode);
        GMSExpression<T> parsed = parser.parse(expression);
        int x = x2 - x1 + 1, y = y2 - y1 + 1, z = z2 - z1 + 1;
        Object[][][] res = new Object[x][y][z];
        for (int i = 0; i < x; i++) {
            for (int j = 0; j < y; j++) {
                for (int k = 0; k < z; k++) {
                    try {
                        res[i][j][k] = parsed.evaluate(mode.value(x1 + i), mode.value(y1 + j), mode.value(z1 + k));
                    } catch (ArithmeticalException e) {
                        res[i][j][k] = null;
                    }
                }
            }
        }
        return res;
    }
}
