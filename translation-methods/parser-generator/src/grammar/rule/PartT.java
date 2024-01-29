package grammar.rule;

import antlr.InputParser.RuleTermContext;

public class PartT implements Part {
    private final String name, code;
    public PartT(final RuleTermContext ctx) {
        this.name = ctx.T_NAME().toString();
        this.code = "Term v%d = new Term(\"" + name + "\", lexer.value()); res.addChild(v%d);"
                + (name.equals("EPS") ? "" : " lexer.nextToken();");
    }

    @Override public String name() { return name; }
    @Override public String code(int i) { return String.format(code, i, i); }
}
