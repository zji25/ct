package game;

public interface Board {
    Position position();
    GameResult makeMove(Move move);
}
