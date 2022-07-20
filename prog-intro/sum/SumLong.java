import java.util.Scanner;

public class SumLong {
    public static void main(String[] args) {
        long result = 0;
        Scanner in;
        for (int i = 0; i < args.length; i++) {
            in = new Scanner(args[i]);
            while (in.hasNextLong()) {
                result += in.nextLong();
            }
        }
        System.out.println(result);
    }
}
