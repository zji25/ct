package expression.parser;

public interface CharSource {
    char next();
    boolean hasNext();
    void rollback(int amount);
}