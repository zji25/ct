package game;

public class Move {
    private final int row, col;
    private final Cell val;

    public Move(int row, int col, Cell val) {
        this.row = row;
        this.col = col;
        this.val = val;
    }

    public int row() {
        return row;
    }

    public int col() {
        return col;
    }

    public Cell value() {
        return val;
    }

    @Override
    public String toString() {
        return String.format("move(%s, %d, %d)", val, row + 1, col + 1);
    }
}