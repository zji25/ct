package pcms;

// didn't pass
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class H {
    private static int nextInt(BufferedReader reader) throws IOException {
        char ch;
        while (Character.isWhitespace(ch = (char)reader.read()));
        int res = 0;
        while (!Character.isWhitespace(ch)) {
            res = res * 10 + ch - '0';
            ch = (char) reader.read();
        }
        return res;
    }
    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        StringBuilder builder = new StringBuilder();
        int n1, n2, counter, e, b, max_a;
        int[] a;
        n1 = nextInt(in);
        a = new int[n1];
        max_a = 0;
        for (int i = 0; i < n1; i++){
            a[i] = nextInt(in);
            if (a[i] > max_a) {
                max_a = a[i];
            }
        }
        n2 = nextInt(in);
        for (int i = 0; i < n2; i++){
            b = nextInt(in);
            if (max_a > b) {
                builder.append("Impossible");
            } else {
                counter = 0;
                e = 0;
                for (int j = 0; j < n1; j++) {
                    if (b < a[j] + e) {
                        e = a[j];
                        counter++;
                        if (j == n1 - 1) {
                            counter++;
                        }
                    } else if (b == a[j] + e) {
                        e = 0;
                        counter++;
                    } else {
                        e += a[j];
                        if (j == n1 - 1) {
                            counter++;
                        }
                    }
                }
                builder.append(counter);
            }
            builder.append('\n');
        }
        System.out.println(builder);
    }
}