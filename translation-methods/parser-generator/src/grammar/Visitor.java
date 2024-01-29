package grammar;

import antlr.InputBaseVisitor;
import antlr.InputParser;
import antlr.InputParser.*;
import grammar.rule.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import static grammar.Common.*;

public class Visitor extends InputBaseVisitor<Object> {
    private final List<Term> terms = new ArrayList<>();
    private final List<NonTerm> nonTerms = new ArrayList<>();
    private String code;

    public List<Term> terms() { return terms; }
    public List<NonTerm> nonTerms() { return nonTerms; }
    public String code() { return code; }
    public static class Term {
        private final String name;
        private final String regex;
        public Term(final TermContext ctx) {
            this.name = ctx.T_NAME().toString();
            this.regex = removeBrackets(ctx.REGEX().toString());
        }
        public String name() { return name; }
        public String regex() { return regex; }
    }
    public static class NonTerm {
        private final String name, node, inh, synt;
        private final List<List<Part>> rules;
        public NonTerm(final RuleContext ctx, final List<List<Part>> rules) {
            this.name = ctx.NT_NAME().toString();
            this.node = firstUpper(name);
            this.inh = removeBrackets(ctx.inh() == null ? null : ctx.inh().ATTR().toString());
            this.synt = Arrays.stream(
                    removeBrackets(ctx.synt() == null ? null : ctx.synt().ATTR().toString()).split(", ")
                    ).map(x -> x + "; ").collect(Collectors.joining());
            this.rules = rules;
        }
        public String name() { return name; }
        public String node() { return node; }
        public String inh() { return inh; }
        public String synt() { return synt; }
        public List<List<Part>> rules() { return rules; }
    }

    @Override public Object visitProg(final ProgContext ctx) {
        visitChildren(ctx);
        return null;
    }

    @Override public Object visitRule(final RuleContext ctx) {
        @SuppressWarnings("unchecked")
        List<List<Part>> rules = ctx.part().stream().map(this::visit).map(x -> (List<Part>) x).toList();
        nonTerms.add(new NonTerm(ctx, rules));
        return null;
    }

    @Override public List<Part> visitNonEps(final NonEpsContext ctx) {
        return ctx.nonTermRule().stream().map(this::visit).map(x -> (Part) x).toList();
    }

    @Override public Part visitRuleCode(final RuleCodeContext ctx) { return new PartCode(ctx); }
    @Override public Part visitRuleNonTerm(final RuleNonTermContext ctx) { return new PartNT(ctx); }
    @Override public Part visitRuleTerm(final RuleTermContext ctx) { return new PartT(ctx); }

    @Override public Object visitTerm(final TermContext ctx) {
        terms.add(new Term(ctx));
        return null;
    }
    @Override public Object visitCode(final InputParser.CodeContext ctx) {
        this.code = removeBrackets(ctx.EXTRA_CODE().toString(), 3);
        return null;
    }
}
