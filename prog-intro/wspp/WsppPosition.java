package wspp;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class WsppPosition {
    private static final StringBuilder builder = new StringBuilder();
    private static int str_pos = 0;
    private static int col_pos = 0;
    private static List<String> array = new ArrayList<>();

    private static void extracted(Map<String, List<String>> m) {
        col_pos++;
        String currentWord = builder.toString();
        builder.setLength(0);
        if (m.containsKey(currentWord)) {
            array = m.get(currentWord);
            int curr = Integer.parseInt(array.get(0));
            array.set(0, String.valueOf(curr + 1));
        } else {
            array.add("1");
        }
        array.add(str_pos + ":" + col_pos);
        m.put(currentWord, new ArrayList<>(array));
        array.clear();
    }

    public static void main(String[] args) throws IOException {
        Map<String, List<String>> hash = new LinkedHashMap<>();
        MyScanner in = new MyScanner(args[0], "UTF-8");
        while (in.hasNextLine()) {
            MyScanner scan = new MyScanner(in.nextLine());
            str_pos++;
            int с = scan.read();
            while (с != -1) {
                char s = (char) с;
                if (Character.isLetter(s) || Character.getType(s) == Character.DASH_PUNCTUATION || s == '\'') {
                    builder.append(Character.toLowerCase(s));
                } else if (!builder.isEmpty()) {
                    extracted(hash);
                }
                с = scan.read();
            }
            if (!builder.isEmpty()) {
                extracted(hash);
            }
            col_pos = 0;
            scan.close();
        }
        in.close();
        try (BufferedWriter writer = new BufferedWriter(new FileWriter(args[1], StandardCharsets.UTF_8))) {
            for (String i : hash.keySet()) {
                array = hash.get(i);
                writer.write(i);
                for (String s : array) {
                    writer.write(" " + s);
                }
                writer.newLine();
            }
            writer.close();
        } catch (IOException e) {
            System.out.println("output error: " + e.getMessage());
        }
        array.clear();
        str_pos = 0;
    }
}

