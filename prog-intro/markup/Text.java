package markup;

public class Text implements InParagraph {
    private String text;

    public Text(String text) {
        this.text = text;
    }

    @Override
    public void toMarkdown(StringBuilder result) {
        result.append(text);
    }

    @Override
    public void toBBCode(StringBuilder result) {
        result.append(text);
    }
}
