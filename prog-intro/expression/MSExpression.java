package expression;

public interface MSExpression extends Expression, TripleExpression {
    int priority();
    boolean isAssociative();
}
