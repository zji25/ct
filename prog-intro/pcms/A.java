package pcms;

import java.util.Scanner;

public class A {
    public static void main(String[] args) {
        int a, b, n, res;
        Scanner in = new Scanner(System.in);
        a = in.nextInt();
        b = in.nextInt();
        n = in.nextInt();
        res = ((int) (2 * Math.ceil((double) (n - b)/(b - a)) + 1));
        System.out.println(res);
    }
}
