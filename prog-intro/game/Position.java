package game;

public interface Position {
    Cell turn();
    boolean isValid(Move move);
}