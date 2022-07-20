package game;

import java.util.Arrays;
import java.util.Map;

public class MNKBoard implements Board, Position {
    private static final Map<Cell, String> CELL_TO_STRING = Map.of(
            Cell.E, ".",
            Cell.X, "X",
            Cell.O, "0"
    );

    private final Cell[][] field;
    private Cell turn;
    private final int m, n, k, spaceM, spaceN;
    private int filled;
    int[][] ways = new int[][] {new int[]{1, 0}, new int[]{0, 1}, new int[]{1, 1}, new int[]{-1, 1}};

    MNKBoard(int m, int n, int k) {
        this.m = m;
        this.n = n;
        this.k = k;
        field = new Cell[m][n];
        for (Cell[] row : field) {
            Arrays.fill(row, Cell.E);
        }
        turn = Cell.X;
        spaceM = Integer.toString(m).length();
        spaceN = Integer.toString(n).length();
    }

    @Override
    public Cell turn() {
        return turn;
    }

    @Override
    public Position position() {
        return this;
    }

    @Override
    public GameResult makeMove(Move move) {
        if (!isValid(move)) {
            return GameResult.LOOSE;
        }
        field[move.row()][move.col()] = move.value();
        filled++;
        if (checkWin(move)) {
            return GameResult.WIN;
        }
        if (checkDraw()) {
            return GameResult.DRAW;
        }
        turn = turn == Cell.X ? Cell.O : Cell.X;
        return GameResult.UNKNOWN;
    }

    private boolean checkDraw() {
        return filled == m * n;
    }

    private boolean checkWin(Move move) {
        int row = move.row();
        int col = move.col();
        for (int i = 0; i < 4; i++) {
            if (1 + count(row, col, ways[i][0], ways[i][1])
                    + count(row, col, -ways[i][0], -ways[i][1]) >= k) {
                return true;
            }
        }
        return false;
    }

    private int count(int row, int col, int x, int y) {
        int count = 0;
        while (0 <= row + x && row + x < m
                && 0 <= col + y && col + y < n
                && field[row + x][col + y] == turn) {
                count++;
                row += x;
                col += y;
        }
        return count;
    }

    public boolean isValid(final Move move) {
        return 0 <= move.row() && move.row() < m
                && 0 <= move.col() && move.col() < n
                && field[move.row()][move.col()] == Cell.E
                && turn == move.value();
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder(" ".repeat(spaceM + 1));
        for (int i = 1; i <= n; i++) {
            sb.append(i).append(" ".repeat(spaceN - Integer.toString(i).length() + 1));
        }
        for (int i = 1; i <= m; i++) {
            sb.append(System.lineSeparator()).append(i).append(" ".repeat(spaceM - Integer.toString(i).length() + 1));
            for (Cell cell : field[i - 1]) {
                sb.append(CELL_TO_STRING.get(cell)).append(" ".repeat(spaceN));
            }
        }
        return sb.toString();
    }
}