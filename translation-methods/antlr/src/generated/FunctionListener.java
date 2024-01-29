package generated;// Generated from src/Function.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link FunctionParser}.
 */
public interface FunctionListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link FunctionParser#prog}.
	 * @param ctx the parse tree
	 */
	void enterProg(FunctionParser.ProgContext ctx);
	/**
	 * Exit a parse tree produced by {@link FunctionParser#prog}.
	 * @param ctx the parse tree
	 */
	void exitProg(FunctionParser.ProgContext ctx);
	/**
	 * Enter a parse tree produced by {@link FunctionParser#function}.
	 * @param ctx the parse tree
	 */
	void enterFunction(FunctionParser.FunctionContext ctx);
	/**
	 * Exit a parse tree produced by {@link FunctionParser#function}.
	 * @param ctx the parse tree
	 */
	void exitFunction(FunctionParser.FunctionContext ctx);
	/**
	 * Enter a parse tree produced by {@link FunctionParser#modifier}.
	 * @param ctx the parse tree
	 */
	void enterModifier(FunctionParser.ModifierContext ctx);
	/**
	 * Exit a parse tree produced by {@link FunctionParser#modifier}.
	 * @param ctx the parse tree
	 */
	void exitModifier(FunctionParser.ModifierContext ctx);
	/**
	 * Enter a parse tree produced by {@link FunctionParser#type}.
	 * @param ctx the parse tree
	 */
	void enterType(FunctionParser.TypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FunctionParser#type}.
	 * @param ctx the parse tree
	 */
	void exitType(FunctionParser.TypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FunctionParser#parameters}.
	 * @param ctx the parse tree
	 */
	void enterParameters(FunctionParser.ParametersContext ctx);
	/**
	 * Exit a parse tree produced by {@link FunctionParser#parameters}.
	 * @param ctx the parse tree
	 */
	void exitParameters(FunctionParser.ParametersContext ctx);
	/**
	 * Enter a parse tree produced by {@link FunctionParser#parameter}.
	 * @param ctx the parse tree
	 */
	void enterParameter(FunctionParser.ParameterContext ctx);
	/**
	 * Exit a parse tree produced by {@link FunctionParser#parameter}.
	 * @param ctx the parse tree
	 */
	void exitParameter(FunctionParser.ParameterContext ctx);
	/**
	 * Enter a parse tree produced by {@link FunctionParser#block}.
	 * @param ctx the parse tree
	 */
	void enterBlock(FunctionParser.BlockContext ctx);
	/**
	 * Exit a parse tree produced by {@link FunctionParser#block}.
	 * @param ctx the parse tree
	 */
	void exitBlock(FunctionParser.BlockContext ctx);
	/**
	 * Enter a parse tree produced by {@link FunctionParser#statement}.
	 * @param ctx the parse tree
	 */
	void enterStatement(FunctionParser.StatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link FunctionParser#statement}.
	 * @param ctx the parse tree
	 */
	void exitStatement(FunctionParser.StatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link FunctionParser#if_statement}.
	 * @param ctx the parse tree
	 */
	void enterIf_statement(FunctionParser.If_statementContext ctx);
	/**
	 * Exit a parse tree produced by {@link FunctionParser#if_statement}.
	 * @param ctx the parse tree
	 */
	void exitIf_statement(FunctionParser.If_statementContext ctx);
	/**
	 * Enter a parse tree produced by {@link FunctionParser#while_loop}.
	 * @param ctx the parse tree
	 */
	void enterWhile_loop(FunctionParser.While_loopContext ctx);
	/**
	 * Exit a parse tree produced by {@link FunctionParser#while_loop}.
	 * @param ctx the parse tree
	 */
	void exitWhile_loop(FunctionParser.While_loopContext ctx);
	/**
	 * Enter a parse tree produced by {@link FunctionParser#condition}.
	 * @param ctx the parse tree
	 */
	void enterCondition(FunctionParser.ConditionContext ctx);
	/**
	 * Exit a parse tree produced by {@link FunctionParser#condition}.
	 * @param ctx the parse tree
	 */
	void exitCondition(FunctionParser.ConditionContext ctx);
	/**
	 * Enter a parse tree produced by {@link FunctionParser#assignment}.
	 * @param ctx the parse tree
	 */
	void enterAssignment(FunctionParser.AssignmentContext ctx);
	/**
	 * Exit a parse tree produced by {@link FunctionParser#assignment}.
	 * @param ctx the parse tree
	 */
	void exitAssignment(FunctionParser.AssignmentContext ctx);
	/**
	 * Enter a parse tree produced by {@link FunctionParser#return}.
	 * @param ctx the parse tree
	 */
	void enterReturn(FunctionParser.ReturnContext ctx);
	/**
	 * Exit a parse tree produced by {@link FunctionParser#return}.
	 * @param ctx the parse tree
	 */
	void exitReturn(FunctionParser.ReturnContext ctx);
	/**
	 * Enter a parse tree produced by {@link FunctionParser#literal}.
	 * @param ctx the parse tree
	 */
	void enterLiteral(FunctionParser.LiteralContext ctx);
	/**
	 * Exit a parse tree produced by {@link FunctionParser#literal}.
	 * @param ctx the parse tree
	 */
	void exitLiteral(FunctionParser.LiteralContext ctx);
}