package md2html;

import java.io.*;
import java.nio.charset.StandardCharsets;

public class Md2Html {
    public static void main(String[] args) {
        String s = "";
        StringBuilder text = new StringBuilder();
        StringBuilder result = new StringBuilder();

        try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(args[0]), StandardCharsets.UTF_8))) {
            while (s != null && (s = reader.readLine()) != null) {
                while (s != null && s.length() != 0) {
                    text.append(s).append('\n');
                    s = reader.readLine();
                }
                if (text.length() > 0) {
                    text.setLength(text.length() - 1);
                    new Parser(text).toHtml(result);
                    result.append('\n');
                    text.setLength(0);
                }
            }
        } catch (FileNotFoundException e) {
            System.out.println("input file not found: " + e.getMessage());
        } catch (IOException e) {
            System.out.println("error while reading input: " + e.getMessage());
        }

        try (BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), StandardCharsets.UTF_8))) {
            writer.write(result.toString());
        } catch (FileNotFoundException e) {
            System.out.println("output file not found: " + e.getMessage());
        } catch (IOException e) {
            System.out.println("error while writing output: " + e.getMessage());
        }
    }
}