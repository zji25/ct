import generated.*;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;

public class HTMLFunctionListener implements FunctionListener {
    private final StringBuilder builder;
    private int countTabs = 0;
    private int prev = -1;

    public HTMLFunctionListener() { builder = new StringBuilder(); }

    public StringBuilder getBuilder() { return builder; }

    @Override public void enterProg(FunctionParser.ProgContext ctx) {
        builder.append("""
                <!DOCTYPE html>
                <html>
                <style>
                .orange{color: orange; font-weight: bold;}
                .number{color: blue;}
                .char{color: green;}
                p{font-size: 12px;}
                </style>
                <p style="font-family: 'verdana';">
                """);
    }
    @Override public void exitProg(FunctionParser.ProgContext ctx) { builder.append("</p></html>\n"); }

    @Override public void enterFunction(FunctionParser.FunctionContext ctx) {}
    @Override public void exitFunction(FunctionParser.FunctionContext ctx) { builder.append(NEWLINE); }

    @Override public void enterModifier(FunctionParser.ModifierContext ctx) {}
    @Override public void exitModifier(FunctionParser.ModifierContext ctx) {}

    @Override public void enterType(FunctionParser.TypeContext ctx) {}
    @Override public void exitType(FunctionParser.TypeContext ctx) {}

    @Override public void enterParameters(FunctionParser.ParametersContext ctx) {}
    @Override public void exitParameters(FunctionParser.ParametersContext ctx) {}

    @Override public void enterParameter(FunctionParser.ParameterContext ctx) {}
    @Override public void exitParameter(FunctionParser.ParameterContext ctx) {}

    @Override public void enterBlock(FunctionParser.BlockContext ctx) {
        builder.append(NEWLINE).append(TAB.repeat(countTabs++)).append('{').append(NEWLINE);
    }
    @Override public void exitBlock(FunctionParser.BlockContext ctx) {
        builder.append(TAB.repeat(--countTabs)).append('}').append(NEWLINE).append(NEWLINE);
    }

    @Override public void enterStatement(FunctionParser.StatementContext ctx) {}
    @Override public void exitStatement(FunctionParser.StatementContext ctx) {}

    @Override public void enterIf_statement(FunctionParser.If_statementContext ctx) {}
    @Override public void exitIf_statement(FunctionParser.If_statementContext ctx) {}

    @Override public void enterWhile_loop(FunctionParser.While_loopContext ctx) {}
    @Override public void exitWhile_loop(FunctionParser.While_loopContext ctx) {}

    @Override public void enterCondition(FunctionParser.ConditionContext ctx) {}
    @Override public void exitCondition(FunctionParser.ConditionContext ctx) {}

    @Override public void enterAssignment(FunctionParser.AssignmentContext ctx) {
        builder.append(TAB.repeat(countTabs));
    }
    @Override public void exitAssignment(FunctionParser.AssignmentContext ctx) {}

    @Override public void enterReturn(FunctionParser.ReturnContext ctx) {
        builder.append(TAB.repeat(countTabs));
    }
    @Override public void exitReturn(FunctionParser.ReturnContext ctx) {}

    @Override public void enterLiteral(FunctionParser.LiteralContext ctx) {}
    @Override public void exitLiteral(FunctionParser.LiteralContext ctx) {}

    private final static String NEWLINE = "<br>";
    private final static String TAB = "&emsp;";
    private final static String SPACE = "&nbsp;";

    private void remove() {
        int matchLength = SPACE.length();
        int builderLength = builder.length();
        while (builderLength >= matchLength
                && builder.substring(builderLength - matchLength, builderLength).equals(SPACE)) {
            builder.delete(builderLength - matchLength, builderLength);
            builderLength = builder.length();
        }
    }

    @Override public void visitTerminal(TerminalNode terminalNode) {
        String token = "";
        String text = terminalNode.getText();
        if (prev == FunctionParser.LB_ROUND) remove();
        switch (terminalNode.getSymbol().getType()) {
        case FunctionParser.NUMBER:
            token = String.format("<span class=\"number\">%s</span>%s", text, SPACE);
            break;
        case FunctionParser.IF:
        case FunctionParser.ELIF:
        case FunctionParser.ELSE:
        case FunctionParser.WHILE:
            builder.append(TAB.repeat(countTabs));
            token = String.format("<span class=\"orange\">%s</span>%s", text, SPACE);
            break;
        case FunctionParser.TYPE:
        case FunctionParser.MODIFIER:
        case FunctionParser.RETURN:
        case FunctionParser.BOOLEAN:
            token = String.format("<span class=\"orange\">%s</span>%s", text, SPACE);
            break;
        case FunctionParser.CHAR:
        case FunctionParser.STRING:
            token = String.format("<span class=\"char\">%s</span>%s", text, SPACE);
            break;
        case FunctionParser.SEMICOLON:
            remove();
            token = String.format("%s%s", text, NEWLINE);
            break;
        case FunctionParser.LB_ROUND:
        case FunctionParser.RB_ROUND:
        case FunctionParser.COMMA:
            remove();
            token = String.format("%s%s", text, SPACE);
            break;
        case FunctionParser.ASSIGN:
        case FunctionParser.EQUALS:
        case FunctionParser.NAME:
            token = String.format("%s%s", text, SPACE);
            break;
        default:
        }
        builder.append(token);
        prev = terminalNode.getSymbol().getType();
    }

    @Override public void visitErrorNode(ErrorNode errorNode) {}

    @Override public void enterEveryRule(ParserRuleContext parserRuleContext) {}
    @Override public void exitEveryRule(ParserRuleContext parserRuleContext) {}
}
