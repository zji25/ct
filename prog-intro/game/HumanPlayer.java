package game;

import java.util.NoSuchElementException;
import java.util.Scanner;

class HumanPlayer implements Player {
    private final Scanner in;
    private Scanner line;

    HumanPlayer(Scanner in) {
        this.in = in;
    }

    @Override
    public Move makeMove(Position position) {
        while (true) {
            int x, y;
            System.out.println("current position");
            System.out.println(position);
            System.out.println("enter you move for " + position.turn());
            line = new Scanner(in.nextLine());
            try {
                x = line.nextInt() - 1;
                y = line.nextInt() - 1;
            } catch (NumberFormatException | NoSuchElementException e) {
                System.out.println("move is invalid");
                continue;
            }
            Move move = new Move(x, y, position.turn());
            if (line.hasNext() || !position.isValid(move)) {
                System.out.println("move is invalid");
            } else {
                return move;
            }
        }
    }
}
