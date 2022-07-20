package md2html;

public class Parser {
    private final StringBuilder data;

    Parser(StringBuilder data) {
        this.data = data;
    }

    private int level(StringBuilder text) {
        int position = 0;
        while (text.charAt(position) == '#' && position < text.length()) {
            position++;
        }
        return position > 0 && position < text.length() && Character.isWhitespace(text.charAt(position)) ? position : 0;
    }

    public void toHtml(StringBuilder result) {
        int level = level(data);
        if (level > 0) {
            result.append("<h").append(level).append(">");
            new TextParser(new StringBuilder(data.substring(level + 1))).toHtml(result);
            result.append("</h").append(level).append(">");
            return;
        }
        result.append("<p>");
        new TextParser(data).toHtml(result);
        result.append("</p>");
    }
}