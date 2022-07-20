package md2html;

class SpecialParser {
    private final int data;

    SpecialParser(int data) {
        this.data = data;
    }

    public void toHtml(StringBuilder result) {
        if (data == '<') {
            result.append("&lt;");
        } else if (data == '>') {
            result.append("&gt;");
        } else if (data == '&') {
            result.append("&amp;");
        } else if (data != '\\') {
            result.append((char) data);
        }
    }
}
