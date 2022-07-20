package markup;

import java.util.List;

public class Emphasis extends AbstractElement implements InParagraph {
    public Emphasis(List<InParagraph> elements) {
        super(elements);
    }

    @Override
    public void toMarkdown(StringBuilder result) {
        toMarkdown(result, "*");
    }

    @Override
    public void toBBCode(StringBuilder result) {
        toBBCode(result, "[i]", "[/i]");
    }
}
