import java.util.Scanner;

public class Sum {
    public static void main(String[] args) {
        int result = 0;
        Scanner in;
        for (int i = 0; i < args.length; i++) {
            in = new Scanner(args[i]);
            while (in.hasNextInt()) {
                result += in.nextInt();
            }
        }
        System.out.println(result);
    }
}
