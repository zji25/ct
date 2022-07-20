package expression.exceptions;

import expression.MSExpression;

public class Main {
    public static void main(String[] args) {
        Parser p = new ExpressionParser();
        MSExpression m = (MSExpression) p.parse("4 min -2");
        System.out.println(m.toMiniString());
        System.out.println(m.evaluate(4, 4, 4));
    }
}
