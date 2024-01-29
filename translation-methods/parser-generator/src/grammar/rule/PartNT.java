package grammar.rule;

import antlr.InputParser.RuleNonTermContext;
import antlr.InputParser.InhContext;
import static grammar.Common.*;

public class PartNT implements Part {
    private final String name, code;

    public PartNT(final RuleNonTermContext ctx) {
        this.name = ctx.NT_NAME().toString();
        final InhContext ic = ctx.inh();
        final String inh = removeBrackets(ic == null ? null : (ic.ATTR() == null ? null : ic.ATTR().toString()));
        this.code = firstUpper(name) + " v%d = " + String.format("%s(%s)", name, inh) + "; res.addChild(v%d);";
    }

    @Override public String name() { return name; }
    @Override public String code(int i) { return String.format(code, i, i); }
}
