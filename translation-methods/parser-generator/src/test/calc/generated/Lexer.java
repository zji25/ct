package test.calc.generated;

import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;
import static test.calc.generated.Token.*;

public class Lexer {
    private String input, value;
    private int pos;
    private Token token;
    private static final Map<Token, String> tokenRgx = new HashMap<>() {{
		put(PLUS, "\\+");
		put(MINUS, "-");
		put(MUL, "\\*");
		put(DIV, "/");
		put(NUMBER, "[0-9]{1,13}(\\.[0-9]*)?");
		put(LR, "\\(");
		put(RR, "\\)");
		put(EXP, "\\^");
		put(SIN, "sin");
		put(MAX, "max");
		put(MIN, "min");
		put(COMMA, ",");
		put(POW, "\\^");
		put(EXL2, "!!");
		put(EXL, "!");
		put(LS, "\\[");
		put(RS, "\\]");
		put(LC, "\\{");
		put(RC, "\\}");
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
