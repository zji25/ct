import java.util.LinkedHashMap;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Map;

public class WordStatInput {
    private static void extracted(Map<String, Integer> hash, StringBuilder builder) {
        String currentWord = builder.toString();
        builder.setLength(0);
        if (hash.containsKey(currentWord)) {
            hash.put(currentWord, hash.get(currentWord) + 1);
        } else {
            hash.put(currentWord, 1);
        }
    }

    public static void main(String[] args) {
        Map<String, Integer> hash = new LinkedHashMap<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(args[0], StandardCharsets.UTF_8))) {
            StringBuilder builder = new StringBuilder();
            int intC = reader.read();
            while (intC != -1) {
                char s = (char) intC;
                if (Character.isLetter(s) || Character.getType(s) == Character.DASH_PUNCTUATION || s == '\'') {
                    builder.append(Character.toLowerCase(s));
                } else if (!builder.isEmpty()) {
                    extracted(hash, builder);
                }
                intC = reader.read();
            }
            if (!builder.isEmpty()) {
                extracted(hash, builder);
            }
        } catch (FileNotFoundException e) {
            System.out.println("couldn't find file: " + e.getMessage());
        } catch (IOException e) {
            System.out.println("input error: " + e.getMessage());
        }

        try (BufferedWriter writer = new BufferedWriter(new FileWriter(args[1], StandardCharsets.UTF_8))) {
            for (String i : hash.keySet()) {
                writer.write(i + " " + hash.get(i));
                writer.newLine();
            }
        } catch (IOException e) {
            System.out.println("output error: " + e.getMessage());
        }
    }
}
