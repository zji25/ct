// Generated from src/antlr/Input.g4 by ANTLR 4.13.1
package antlr;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link InputParser}.
 */
public interface InputListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link InputParser#prog}.
	 * @param ctx the parse tree
	 */
	void enterProg(InputParser.ProgContext ctx);
	/**
	 * Exit a parse tree produced by {@link InputParser#prog}.
	 * @param ctx the parse tree
	 */
	void exitProg(InputParser.ProgContext ctx);
	/**
	 * Enter a parse tree produced by {@link InputParser#rule}.
	 * @param ctx the parse tree
	 */
	void enterRule(InputParser.RuleContext ctx);
	/**
	 * Exit a parse tree produced by {@link InputParser#rule}.
	 * @param ctx the parse tree
	 */
	void exitRule(InputParser.RuleContext ctx);
	/**
	 * Enter a parse tree produced by {@link InputParser#inh}.
	 * @param ctx the parse tree
	 */
	void enterInh(InputParser.InhContext ctx);
	/**
	 * Exit a parse tree produced by {@link InputParser#inh}.
	 * @param ctx the parse tree
	 */
	void exitInh(InputParser.InhContext ctx);
	/**
	 * Enter a parse tree produced by {@link InputParser#synt}.
	 * @param ctx the parse tree
	 */
	void enterSynt(InputParser.SyntContext ctx);
	/**
	 * Exit a parse tree produced by {@link InputParser#synt}.
	 * @param ctx the parse tree
	 */
	void exitSynt(InputParser.SyntContext ctx);
	/**
	 * Enter a parse tree produced by the {@code eps}
	 * labeled alternative in {@link InputParser#part}.
	 * @param ctx the parse tree
	 */
	void enterEps(InputParser.EpsContext ctx);
	/**
	 * Exit a parse tree produced by the {@code eps}
	 * labeled alternative in {@link InputParser#part}.
	 * @param ctx the parse tree
	 */
	void exitEps(InputParser.EpsContext ctx);
	/**
	 * Enter a parse tree produced by the {@code nonEps}
	 * labeled alternative in {@link InputParser#part}.
	 * @param ctx the parse tree
	 */
	void enterNonEps(InputParser.NonEpsContext ctx);
	/**
	 * Exit a parse tree produced by the {@code nonEps}
	 * labeled alternative in {@link InputParser#part}.
	 * @param ctx the parse tree
	 */
	void exitNonEps(InputParser.NonEpsContext ctx);
	/**
	 * Enter a parse tree produced by the {@code ruleCode}
	 * labeled alternative in {@link InputParser#nonTermRule}.
	 * @param ctx the parse tree
	 */
	void enterRuleCode(InputParser.RuleCodeContext ctx);
	/**
	 * Exit a parse tree produced by the {@code ruleCode}
	 * labeled alternative in {@link InputParser#nonTermRule}.
	 * @param ctx the parse tree
	 */
	void exitRuleCode(InputParser.RuleCodeContext ctx);
	/**
	 * Enter a parse tree produced by the {@code ruleNonTerm}
	 * labeled alternative in {@link InputParser#nonTermRule}.
	 * @param ctx the parse tree
	 */
	void enterRuleNonTerm(InputParser.RuleNonTermContext ctx);
	/**
	 * Exit a parse tree produced by the {@code ruleNonTerm}
	 * labeled alternative in {@link InputParser#nonTermRule}.
	 * @param ctx the parse tree
	 */
	void exitRuleNonTerm(InputParser.RuleNonTermContext ctx);
	/**
	 * Enter a parse tree produced by the {@code ruleTerm}
	 * labeled alternative in {@link InputParser#nonTermRule}.
	 * @param ctx the parse tree
	 */
	void enterRuleTerm(InputParser.RuleTermContext ctx);
	/**
	 * Exit a parse tree produced by the {@code ruleTerm}
	 * labeled alternative in {@link InputParser#nonTermRule}.
	 * @param ctx the parse tree
	 */
	void exitRuleTerm(InputParser.RuleTermContext ctx);
	/**
	 * Enter a parse tree produced by {@link InputParser#term}.
	 * @param ctx the parse tree
	 */
	void enterTerm(InputParser.TermContext ctx);
	/**
	 * Exit a parse tree produced by {@link InputParser#term}.
	 * @param ctx the parse tree
	 */
	void exitTerm(InputParser.TermContext ctx);
	/**
	 * Enter a parse tree produced by {@link InputParser#code}.
	 * @param ctx the parse tree
	 */
	void enterCode(InputParser.CodeContext ctx);
	/**
	 * Exit a parse tree produced by {@link InputParser#code}.
	 * @param ctx the parse tree
	 */
	void exitCode(InputParser.CodeContext ctx);
}