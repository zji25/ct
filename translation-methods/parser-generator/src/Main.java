import grammar.Generator;
import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {
        String grammar = "src/test/calc";
        new Generator(grammar).generate();
    }
}
