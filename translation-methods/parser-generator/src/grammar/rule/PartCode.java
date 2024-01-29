package grammar.rule;

import antlr.InputParser.RuleCodeContext;

import static grammar.Common.removeBrackets;

public class PartCode implements Part {
    private final String code;
    public PartCode(final RuleCodeContext code) { this.code = removeBrackets(code.CODE().toString()); }

    @Override public String name() { return null; }
    @Override public String code(int i) { return code; }
}
