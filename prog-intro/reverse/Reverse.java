import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Reverse {
    public static void main (String[] args) throws IOException {
        MyScanner scanner = new MyScanner(System.in);
        List<List<Integer>> array = new ArrayList<>();
        while (scanner.hasNextLine()) {
            MyScanner lineScanner = new MyScanner(scanner.nextLine());
            List<Integer> a = new ArrayList<>();
            while (lineScanner.hasNextInt()) {
                a.add(lineScanner.nextInt());
            }
            array.add(a);
            lineScanner.close();
        }
        scanner.close();
        for (int i = array.size() - 1; i >= 0; i--) {
            for (int j = array.get(i).size() - 1; j >= 0; j--) {
                System.out.print(array.get(i).get(j).toString() + " ");
            }
            System.out.println();
        }
    }
}