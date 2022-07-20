package game;

class TwoPlayerGame {
    private final Board board;
    private final Player player1;
    private final Player player2;

    TwoPlayerGame(Board board, Player player1, Player player2) {
        this.board = board;
        this.player1 = player1;
        this.player2 = player2;
    }

    public int play(boolean log) {
        while (true) {
            final int result1 = makeMove(player1, 1, log);
            if (result1 != -1)  {
                return result1;
            }
            final int result2 = makeMove(player2, 2, log);
            if (result2 != -1)  {
                return result2;
            }
        }
    }

    private int makeMove(Player player, int no, boolean log) {
        final Move move = player.makeMove(board.position());
        final GameResult result = board.makeMove(move);
        if (log) {
            System.out.println("player: " + no);
            System.out.println(move);
            System.out.println(board);
            System.out.println("result: " + result);
        }
        switch (result) {
            case WIN:
                return no;
            case LOOSE:
                return 3 - no;
            case DRAW:
                return 0;
            case UNKNOWN:
                return -1;
            default:
                throw new AssertionError("unknown makeMove result " + result);
        }
    }
}
