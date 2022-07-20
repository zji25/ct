package pcms;

import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class M {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int t = in.nextInt();
        int n, result;
        int[] a;
        Map<Integer, Integer> c = new HashMap<>();
        for (int o = 0; o < t; o++){
            n = in.nextInt();
            result = 0;
            a = new int[n];
            for (int q = 0; q < n; q++){
                a[q] = in.nextInt();
            }
            for (int j = 1; j < n - 1; j++){
                for (int i = j - 1; i >= 0; i--){
                    if (c.containsKey(a[j] - a[i])){
                        c.put(a[j] - a[i], c.get(a[j] - a[i]) + 1);
                    } else {
                        c.put(a[j] - a[i], 1);
                    }
                }
                for (int k = j + 1; k < n; k++){
                    if (c.containsKey(a[k] - a[j])){
                        result += c.get(a[k] - a[j]);
                    }
                }
                c.clear();
            }
            System.out.println(result);
        }
    }
}

