import generated.*;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;


public class Main {
    public static void main(String[] args) throws IOException {
        HTMLFunctionListener listener = new HTMLFunctionListener();
        new ParseTreeWalker().walk(
                listener,
                new FunctionParser(new CommonTokenStream(new FunctionLexer(CharStreams.fromFileName("test/input"))))
                        .prog()
        );
        BufferedWriter writer = new BufferedWriter(new FileWriter("test/output.html"));
        writer.write(listener.getBuilder().toString());
        writer.close();
    }
}
