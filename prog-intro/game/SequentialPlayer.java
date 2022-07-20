package game;

class SequentialPlayer implements Player {
    private final int m, n;

    SequentialPlayer(int m, int n) {
        this.m = m;
        this.n = n;
    }
    @Override
    public Move makeMove(Position position) {
        for (int r = 0; r < m; r++) {
            for (int c = 0; c < n; c++) {
                final Move move = new Move(r, c, position.turn());
                if (position.isValid(move)) {
                    return move;
                }
            }
        }
        throw new AssertionError("no valid moves");
    }
}