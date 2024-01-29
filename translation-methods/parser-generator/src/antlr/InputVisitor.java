// Generated from src/antlr/Input.g4 by ANTLR 4.13.1
package antlr;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link InputParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface InputVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link InputParser#prog}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProg(InputParser.ProgContext ctx);
	/**
	 * Visit a parse tree produced by {@link InputParser#rule}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRule(InputParser.RuleContext ctx);
	/**
	 * Visit a parse tree produced by {@link InputParser#inh}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInh(InputParser.InhContext ctx);
	/**
	 * Visit a parse tree produced by {@link InputParser#synt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSynt(InputParser.SyntContext ctx);
	/**
	 * Visit a parse tree produced by the {@code eps}
	 * labeled alternative in {@link InputParser#part}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEps(InputParser.EpsContext ctx);
	/**
	 * Visit a parse tree produced by the {@code nonEps}
	 * labeled alternative in {@link InputParser#part}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNonEps(InputParser.NonEpsContext ctx);
	/**
	 * Visit a parse tree produced by the {@code ruleCode}
	 * labeled alternative in {@link InputParser#nonTermRule}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRuleCode(InputParser.RuleCodeContext ctx);
	/**
	 * Visit a parse tree produced by the {@code ruleNonTerm}
	 * labeled alternative in {@link InputParser#nonTermRule}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRuleNonTerm(InputParser.RuleNonTermContext ctx);
	/**
	 * Visit a parse tree produced by the {@code ruleTerm}
	 * labeled alternative in {@link InputParser#nonTermRule}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRuleTerm(InputParser.RuleTermContext ctx);
	/**
	 * Visit a parse tree produced by {@link InputParser#term}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTerm(InputParser.TermContext ctx);
	/**
	 * Visit a parse tree produced by {@link InputParser#code}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCode(InputParser.CodeContext ctx);
}