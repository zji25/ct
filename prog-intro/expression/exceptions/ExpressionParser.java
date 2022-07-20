package expression.exceptions;

import expression.*;
import expression.checked.*;
import expression.unchecked.*;

import static expression.Term.*;

public class ExpressionParser extends BaseParser implements Parser {
    @Override
    public MSExpression parse(String expression) {
        source = new StringSource(expression);
        take();
        MSExpression result = parseExpression(MIN_PRIORITY);
        skip();
        if (current == END) {
            return result;
        }
        throw new UnexpectedCharacterException("end", Character.toString(current));
    }

    private MSExpression parseExpression(int search) {
        if (search < MAX_PRIORITY) {
            MSExpression result = parseExpression(search + 1);
            skip();
            while (getPriority() == search) {
                if (getPriority() == op2priority.get(MINIMUM)) {
                    take();
                    if (test("in")) {
                        result = new Min(result, parseExpression(search + 1));
                    } else if (test("ax")) {
                        result = new Max(result, parseExpression(search + 1));
                    }
                } else {
                    String symbol = Character.toString(current);
                    take();
                    if (search == DOUBLES_PRIORITY) {
                        symbol = symbol.repeat(2);
                        take();
                    }
                    result = apply(symbol, result, parseExpression(search + 1));
                }
                skip();
            }
            return result;
        }
        skip();
        if (test('(')) {
            MSExpression result = parseExpression(MIN_PRIORITY);
            expect(')');
            return result;
        }
        if (between('x', 'z')) {
            return new Variable(Character.toString(take()));
        }
        if (Character.isDigit(current)) {
            return new Const(nextInt(false));
        }
        if (test('-')) {
            if (Character.isDigit(current)) {
                return new Const(nextInt(true));
            }
            return new CheckedNegate(parseExpression(MAX_PRIORITY));
        }
        if (test("l0")) {
            inspect(' ', '(');
            return new LZeroes(parseExpression(MAX_PRIORITY));
        }
        if (test("t0")) {
            inspect(' ', '(');
            return new TZeroes(parseExpression(MAX_PRIORITY));
        }
        if (test("abs")) {
            inspect(' ', '(');
            return new CheckedAbs(parseExpression(MAX_PRIORITY));
        }
        throw new MissingArgumentException();
    }

    private MSExpression apply(String operation, MSExpression first, MSExpression second) {
        return switch (operation) {
            case "+" -> new CheckedAdd(first, second);
            case "-" -> new CheckedSubtract(first, second);
            case "*" -> new CheckedMultiply(first, second);
            case "/" -> new CheckedDivide(first, second);
            case "**" -> new CheckedPow(first, second);
            case "//" -> new CheckedLog(first, second);
            default -> throw new ExpressionException(); // imp case
        };
    }

    private int getPriority() {
        if (doublingSymbols.contains(current)) {
            if (source.checkNext(current)) {
                return symbol2priority.get(Character.toString(current).repeat(2));
            }
        }
        if (symbol2priority.containsKey(Character.toString(current))) {
            return symbol2priority.get(Character.toString(current));
        }
        return 100;
    }
}