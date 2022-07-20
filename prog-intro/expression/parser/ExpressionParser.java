package expression.parser;

import expression.*;
import expression.unchecked.*;

import static expression.Term.*;

public class ExpressionParser extends BaseParser implements Parser {
    @Override
    public MSExpression parse(String expression) {
        source = new StringSource(expression);
        take();
        MSExpression result = parseExpression(MIN_PRIORITY);
        skip();
        if (current == END) return result;
        throw error("invalid expression");
    }

    private MSExpression parseExpression(int search) {
        if (search < MAX_PRIORITY) {
            MSExpression result = parseExpression(search + 1);
            skip();
            while (getPriority() == search) {
                char symbol = current;
                take();
                result = apply(symbol, result, parseExpression(search + 1));
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
            return new Negate(parseExpression(MAX_PRIORITY));
        }
        if (test('l')) {
            expect('0');
            return new LZeroes(parseExpression(MAX_PRIORITY));
        }
        if (test('t')) {
            expect('0');
            return new TZeroes(parseExpression(MAX_PRIORITY));
        }
        throw error("invalid expression");
    }

    private MSExpression apply(char symbol, MSExpression first, MSExpression second) {
        return switch (symbol) {
            case '+' -> new Add(first, second);
            case '-' -> new Subtract(first, second);
            case '*' -> new Multiply(first, second);
            case '/' -> new Divide(first, second);
            default -> throw error("no such operation");
        };
    }

    private int getPriority() {
        if (symbol2priority.containsKey(current)) {
            return symbol2priority.get(current);
        }
        return 100;
    }
}
