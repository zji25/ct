package pcms;

import java.util.Scanner;

public class I {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        int xl = Integer.MAX_VALUE;
        int xr = Integer.MIN_VALUE;
        int yl = Integer.MAX_VALUE;
        int yr = Integer.MIN_VALUE;
        int x, y, h;
        for (int i = 0; i < n; i++) {
            x = in.nextInt();
            y = in.nextInt();
            h = in.nextInt();
            if (x - h < xl) {
                xl = x - h;
            }
            if (x + h > xr) {
                xr = x + h;
            }
            if (y - h < yl) {
                yl = y - h;
            }
            if (y + h > yr) {
                yr = y + h;
            }
        }
        int resH = (int) Math.ceil((double) Math.max(xr - xl, yr - yl) / 2);
        int resX = (xl + xr) / 2;
        int resY = (yl + yr) / 2;
        System.out.print(resX + " ");
        System.out.print(resY + " ");
        System.out.println(resH);
    }
}

