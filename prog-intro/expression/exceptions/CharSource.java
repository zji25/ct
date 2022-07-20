package expression.exceptions;

public interface CharSource {
    char next();
    boolean hasNext();
    boolean checkNext(char a);
}