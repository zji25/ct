package game;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        final int m, n, k, rounds;
        m = 13;
        n = 13;
        k = 2;
        rounds = 1;
        Player player1 = new RandomPlayer(m, n);
        Player player2 = new HumanPlayer(new Scanner(System.in));
        new Rounds(player1, player2, m, n, k, rounds).play();
    }
}