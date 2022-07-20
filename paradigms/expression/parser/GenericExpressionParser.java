package expression.parser;

import expression.*;
import expression.parser_exceptions.MissingArgumentException;
import expression.parser_exceptions.ParsingException;
import expression.parser_exceptions.UnexpectedCharactersException;
import expression.op_generic.*;
import expression.modes.Modes;

import static expression.op_generic.GTerm.*;

public class GenericExpressionParser<T> extends BaseParser implements GenericParser<T> {
    private final Modes<T> mode;
    public GenericExpressionParser(final Modes<T> mode) {
        this.mode = mode;
    }

    @Override
    public GMSExpression<T> parse(String expression) {
        source = new StringSource(expression);
        take();
        GMSExpression<T> result = parseExpression(MIN_PRIORITY);
        skip();
        if (current == END) return result;
        throw new UnexpectedCharactersException("end", Character.toString(current));
    }

    private GMSExpression<T> parseExpression(int search) {
        if (search < MAX_PRIORITY) {
            GMSExpression<T> result = parseExpression(search + 1);
            skip();
            while (getPriority() == search) {
                if (search == op2priority.get(MINIMUM)) {
                    if (test("min")) result = new GMin<>(result, parseExpression(search + 1), mode);
                    else if (test("max")) result = new GMax<>(result, parseExpression(search + 1), mode);
                } else result = apply(take(), result, parseExpression(search + 1));
                skip();
            }
            return result;
        }
        skip();
        if (test('(')) {
            GMSExpression<T> result = parseExpression(MIN_PRIORITY);
            expect(')');
            return result;
        }
        if (between('x', 'z')) return new GVariable<>(Character.toString(take()));
        if (Character.isDigit(current)) return new GConst<>(mode.value(nextNumber(false)));
        if (test('-')) {
            if (Character.isDigit(current)) return new GConst<>(mode.value(nextNumber(true)));
            return new GNegate<>(parseExpression(MAX_PRIORITY), mode);
        }
        if (test("count")) return new GCount<>(parseExpression(MAX_PRIORITY), mode);
        throw new MissingArgumentException();
    }

    private GMSExpression<T> apply(char symbol, GMSExpression<T> first, GMSExpression<T> second) {
        return switch (symbol) {
            case '+' -> new GAdd<>(first, second, mode);
            case '-' -> new GSubtract<>(first, second, mode);
            case '*' -> new GMultiply<>(first, second, mode);
            case '/' -> new GDivide<>(first, second, mode);
            default -> throw new ParsingException("no such operation: " + symbol);
        };
    }

    private int getPriority() {
        if (symbol2priority.containsKey(current)) return symbol2priority.get(current);
        return 100;
    }
}
