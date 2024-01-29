package grammar;

import antlr.*;
import com.google.common.collect.Lists;
import dnl.utils.text.table.TextTable;
import grammar.rule.*;
import grammar.Visitor.*;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

public class Generator {
    private final Path grammar, tests, generated;
    private final String packageName;
    private boolean genTests = true;
    private final Map<String, Set<String>> FIRST = new HashMap<>();
    private final Map<String, Set<String>> FOLLOW = new HashMap<>();

    public Generator(final String directory) throws IOException {
        final Path path = Path.of(directory);
        if (!Files.exists(path)) throw new FileNotFoundException(String.format("no such dir: %s", directory));
        this.grammar = path.resolve("grammar.txt");
        if (!Files.exists(grammar)) throw new FileNotFoundException("input dir must contain a file named grammar.txt");
        this.tests = path.resolve("tests.txt");
        if (!Files.exists(tests)) genTests = false;
        this.generated = path.resolve("generated");
        if (!Files.exists(generated)) Files.createDirectory(generated);
        this.packageName = generated.subpath(1, 4).toString().replace('/', '.');
    }
    public void generate() throws IOException {
        final ParseTree tree = new InputParser(
                new CommonTokenStream(new InputLexer(CharStreams.fromPath(grammar)))
        ).prog();
        Visitor visitor = new Visitor();
        visitor.visit(tree);
        final List<Term> terms = visitor.terms();
        final List<NonTerm> nonTerms = visitor.nonTerms();

        generateFF(nonTerms);
        if (!checkLL1(nonTerms)) throw new IllegalArgumentException("input grammar is not ll1");
        System.out.println("ll1 grammar");

        generateToken(terms);
        generateLexer(terms);
        generateParser(visitor);
        if (genTests) generateTest();
    }

    private void generateToken(final List<Term> terms) throws IOException {
        try (final BufferedWriter writer = createBW("Token")) {
            writePackage(writer);
            writer.write(String.format("public enum Token {\n\t%sEOF\n}",
                    terms.stream().map(Term::name).map(x -> x + ", ").collect(Collectors.joining())));
        }
    }

    private void generateLexer(final List<Term> terms) throws IOException {
        try (final BufferedWriter writer = createBW("Lexer")) {
            writePackage(writer);
            writer.write(String.format(LEXER_IMPL, packageName,
                terms.stream().map(x -> String.format("\t\tput(%s, \"%s\");", x.name(), x.regex()))
                        .collect(Collectors.joining("\n"))));
        }
    }
    private void generateParser(final Visitor visitor) throws IOException {
        try (final BufferedWriter writer = createBW("Parser")) {
            writePackage(writer);
            writer.write(String.format(PARSER_START_IMPL, String.join("\n\t", visitor.code().split("\n"))));
            for (NonTerm nt : visitor.nonTerms()) writeNT(writer, nt);
            writer.write("}");
        }
    }

    private void writeNT(final BufferedWriter writer, final NonTerm nt) throws IOException {
        writer.write(String.format(
            "\tpublic static class %s extends Nt {\n\t\tpublic %s() { super(\"%s\"); }\n\t\t%s\n\t}\n",
            nt.node(), nt.node(), nt.name(), nt.synt()
        ));
        writer.write(String.format(
            "\tprivate %s %s(%s) throws ParseException {\n\t\t%s res = new %s();\n\t\tswitch (lexer.token()) {\n",
            nt.node(), nt.name(), nt.inh(), nt.node(), nt.node()
        ));
        final Set<String> followA = FOLLOW.get(nt.name());
        for (List<Part> rule : nt.rules()) {
            Set<String> first1 = first(rule);
            if (first1.remove("EPS")) first1.addAll(followA);
            StringBuilder cas = new StringBuilder();
            cas.append(String.format("\t\t\tcase %s -> {\n", String.join(", ", first1)));
            for (int i = 0; i < rule.size(); ++i)
                cas.append(String.format("\t\t\t\t%s\n", rule.get(i).code(i)));
            cas.append("\t\t\t}\n");
            writer.write(cas.toString());
        }
        writer.write("\t\t\tdefault -> throw new ParseException(\"no such rule\", lexer.pos());\n\t\t}\n");
        writer.write("\t\treturn res;\n\t}\n");
    }


    private void generateTest() throws IOException {
        try (final BufferedReader reader = Files.newBufferedReader(tests)) {
            try (final BufferedWriter writer = createBW("ParserTest")) {
                writePackage(writer);
                writer.write(String.format("""                    
                import org.junit.jupiter.api.Assertions;
                import org.junit.jupiter.params.ParameterizedTest;
                import org.junit.jupiter.params.provider.CsvSource;
                import java.text.ParseException;
                                        
                public class ParserTest {
                    private final Parser parser = new Parser();
                                        
                    @ParameterizedTest @CsvSource(value = {
                    %s
                    }, delimiter = ';')
                    public void test(final String input, double expected) {
                        try {
                            Assertions.assertEquals(expected, parser.parse(input).val, 0.0001);
                        } catch (final ParseException e) {
                            throw new RuntimeException(e);
                        }
                    }
                }
                """, reader.lines().map(s -> String.format("\"%s\"", s)).collect(Collectors.joining(",\n\t"))));
            }
        }
    }

    private BufferedWriter createBW(final String name) throws IOException {
        final Path file = generated.resolve(name + ".java");
        Files.deleteIfExists(file);
        Files.createFile(file);
        return Files.newBufferedWriter(file, StandardCharsets.UTF_8);
    }
    private void writePackage(final BufferedWriter writer) throws IOException {
        writer.write(String.format("package %s;\n\n", packageName));
    }

    private static final String EPS = "EPS", EOF = "EOF";
    private static final String LEXER_IMPL =
        """  
        import java.text.ParseException;
        import java.util.HashMap;
        import java.util.Map;
        import java.util.regex.Pattern;
        import static %s.Token.*;
        
        public class Lexer {
            private String input, value;
            private int pos;
            private Token token;
            private static final Map<Token, String> tokenRgx = new HashMap<>() {{
        %s
            }};
            public void set(String input) throws ParseException {
                this.input = input; this.pos = 0; nextToken();
            }
            public Token token() { return token; }
            public String value() { return value; }
            public int pos() { return pos; }
            public void nextToken() throws ParseException {
                while (pos < input.length() && Character.isWhitespace(input.charAt(pos))) ++pos;
                if (pos >= input.length()) { token = Token.EOF; value = null; return; }
                StringBuilder builder = new StringBuilder();
                boolean prev = false;
                while (true) {
                    if (pos == input.length()) { if (prev) return; throw new ParseException("f", pos); }
                    builder.append(input.charAt(pos));
                    boolean found = false;
                    for (Map.Entry<Token, String> entry : tokenRgx.entrySet()) {
                        Pattern pattern = Pattern.compile(entry.getValue());
                        if (pattern.matcher(builder.toString()).matches()) {
                            token = entry.getKey(); value = builder.toString();
                            found = true; break;
                        }
                    }
                    if (!found && prev) return;
                    prev = found;
                    ++pos;
                }
            }
        }
        """;
    private static final String PARSER_START_IMPL =
        """
        import java.text.ParseException;
        import java.util.ArrayList;
                    
        public class Parser {
            private final Lexer lexer = new Lexer();
            public Prog parse(final String input) throws ParseException {
                lexer.set(input);
                return prog();
            }
            %s
            private abstract static class Nt {
                private final String name;
                private final ArrayList<Nt> children = new ArrayList<>();
                public Nt(String name) { this.name = name; }
                public void addChild(Nt nt) { children.add(nt); }
                public String name() { return name; }
                public ArrayList<Nt> children() { return children; }
            }
            private static class Term extends Nt {
                public String value;
                public Term(String name, String value) { super(name); this.value = value; }
            }
        """;

    private void generateFF(final List<NonTerm> nonTerms) {
        boolean changed;
        nonTerms.forEach(nt -> FIRST.put(nt.name(), new HashSet<>()));
        do {
            changed = false;
            for (NonTerm nt : nonTerms) {
                Set<String> set = FIRST.get(nt.name());
                for (List<Part> rule : nt.rules()) {
                    if (rule == null) { if (set.add(EPS)) changed = true; continue; }
                    for (Part part : rule) {
                        if (part instanceof PartT) { if (set.add(part.name())) changed = true; break; }
                        if (part instanceof PartNT) {
                            boolean cont = false;
                            for (String faa : FIRST.get(part.name())) {
                                if (faa.equals(EPS)) cont = true;
                                else if (set.add(faa)) changed = true;
                            }
                            if (!cont) break;
                        }
                    }
                }
            }
        } while (changed);

        nonTerms.forEach(nt -> FOLLOW.put(nt.name(), new HashSet<>()));
        FOLLOW.get("prog").add(EOF);
        do {
            changed = false;
            for (NonTerm nt : nonTerms) {
                for (List<Part> rule : nt.rules()) if (rule != null) {
                    Set<String> fg = new HashSet<>();
                    fg.add(EPS);
                    for (Part part : Lists.reverse(rule)) {
                        if (part instanceof PartCode) continue;
                        if (part instanceof PartNT) {
                            Set<String> set = FOLLOW.get(part.name());
                            int ss = set.size();
                            set.addAll(fg);
                            if (fg.contains(EPS)) { set.remove(EPS); set.addAll(FOLLOW.get(nt.name())); }
                            if (set.size() != ss) changed = true;
                            if (FIRST.get(part.name()).contains(EPS)) fg.addAll(FIRST.get(part.name()));
                            else { fg.clear(); fg.addAll(FIRST.get(part.name())); }
                        } else { fg.clear(); fg.add(part.name()); }
                    }
                }
            }
        } while (changed);
        new TextTable(new String[]{"nt", "FIRST", "FOLLOW"}, FIRST.entrySet().stream().map(entry -> {
            String nt = entry.getKey();
            return new String[]{
                    nt, String.join(" ", entry.getValue()).toLowerCase(), String.join(" ", FOLLOW.get(nt)).toLowerCase()
            };
        }).toList().toArray(new Object[0][])).printTable();
    }


    private Set<String> first(final List<Part> alpha) {
        Set<String> res = new HashSet<>();
        if (alpha.size() == 1 && EPS.equals(alpha.get(0).name())) { res.add(EPS); return res; }
        for (Part part : alpha) {
            if (part instanceof PartT) {
                res.add(part.name());
                break;
            } else if (part instanceof PartNT) {
                boolean foundEps = false;
                for (String s : FIRST.get(part.name())) {
                    if (EPS.equals(s)) foundEps = true;
                    else res.add(s);
                }
                if (!foundEps) break;
            }
        }
        return res;
    }

    private boolean checkLL1(final List<NonTerm> nonTerms) {
        for (NonTerm nt : nonTerms) {
            boolean foundEps = false;
            List<Part> where = null;
            Set<String> allFirsts = new HashSet<>();
            for (List<Part> rule : nt.rules()) {
                for (String f : first(rule)) {
                    if (!allFirsts.add(f)) return false;
                    if (EPS.equals(f)) { foundEps = true; where = rule; }
                }
            }
            if (!foundEps) continue;
            for (List<Part> rule : nt.rules()) if (!rule.equals(where)) {
                Set<String> follow = new HashSet<>(Set.copyOf(FOLLOW.get(nt.name())));
                for (String f : first(rule)) if (!follow.add(f)) return false;
            }
        }
        return true;
    }
}
