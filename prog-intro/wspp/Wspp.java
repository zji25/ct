package wspp;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class Wspp {
    private static final StringBuilder builder = new StringBuilder();
    private static int position = 0;
    private static List<Integer> array = new ArrayList<>();

    private static void extracted(Map<String, List<Integer>> m) {
        position++;
        String currentWord = builder.toString();
        builder.setLength(0);
        if (m.containsKey(currentWord)) {
            array = m.get(currentWord);
            array.set(0, array.get(0) + 1);
        } else {
            array.add(1);
        }
        array.add(position);
        m.put(currentWord, new ArrayList<>(array));
        array.clear();
    }

    public static void main(String[] args) throws IOException {
        Map<String, List<Integer>> hash = new LinkedHashMap<>();
        MyScanner in = new MyScanner(args[0], "UTF-8");
        while (in.hasNextLine()) {
            MyScanner scan = new MyScanner(in.nextLine());
            int intC = scan.read();
            while (intC != -1) {
                char s = (char) intC;
                if (Character.isLetter(s) || Character.getType(s) == Character.DASH_PUNCTUATION || s == '\'') {
                    builder.append(Character.toLowerCase(s));
                } else if (!builder.isEmpty()) {
                    extracted(hash);
                }
                intC = scan.read();
            }
            if (!builder.isEmpty()) {
                extracted(hash);
            }
            scan.close();
        }
        in.close();
        try (BufferedWriter writer = new BufferedWriter(new FileWriter(args[1], StandardCharsets.UTF_8))) {
            for (String i : hash.keySet()) {
                array = hash.get(i);
                writer.write(i);
                for (int j = 0; j < array.size(); j++) {
                    writer.write(" " + array.get(j));
                }
                writer.newLine();
            }
            writer.close();
        } catch (IOException e) {
            System.out.println("output error: " + e.getMessage());
        }
        array.clear();
        position = 0;
    }
}
