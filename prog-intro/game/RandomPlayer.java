package game;

import java.util.Random;

class RandomPlayer implements Player {
    private final Random random = new Random();
    private final int m, n;

    RandomPlayer(int m, int n) {
        this.m = m;
        this.n = n;
    }

    @Override
    public Move makeMove(Position position) {
        while (true) {
            final Move move = new Move(random.nextInt(m), random.nextInt(n), position.turn());
            if (position.isValid(move)) {
                return move;
            }
        }
    }
}
