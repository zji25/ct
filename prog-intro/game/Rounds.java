package game;

class Rounds {
    final int m, n, k, rounds;
    Player player1, player2, playerT;
    int res1, res2, resT, result;
    String a, b, t;

    Rounds(Player player1, Player player2, int m, int n, int k, int rounds) {
        this.player1 = player1;
        this.player2 = player2;
        this.m = m;
        this.n = n;
        this.k = k;
        this.rounds = rounds;
        a = "first";
        b = "second";
    }

    public void play() {
        for (int i = 0; i < rounds; i++) {
            result = new TwoPlayerGame(new MNKBoard(m, n, k), player1, player2).play(true);
            switch (result) {
                case 1 -> {
                    System.out.println("round " + (i + 1) + ": " + a + " player won");
                    res1 += 3;
                }
                case 2 -> {
                    System.out.println("round " + (i + 1) + ": " + b + " player won");
                    res2 += 3;
                }
                case 0 -> {
                    System.out.println("round " + (i + 1) + ": draw");
                    res1++;
                    res2++;
                }
                default -> throw new AssertionError("unknown result " + result);
            }
            swap();
        }
        if (rounds % 2 != 0) {
            swap();
        }
        if (res1 > res2) {
            System.out.print("first player won: ");
        } else if (res1 < res2) {
            System.out.print("second player won: ");
        } else {
            System.out.print("draw: ");
        }
        System.out.println(res1 + "/" + res2);
    }

    private void swap() {
        playerT = player1;
        player1 = player2;
        player2 = playerT;
        resT = res1;
        res1 = res2;
        res2 = resT;
        t = a;
        a = b;
        b = t;
    }
}
