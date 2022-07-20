package markup;

import java.util.List;

public class Strikeout extends AbstractElement implements InParagraph {
    public Strikeout(List<InParagraph> list) {
        super(list);
    }

    @Override
    public void toMarkdown(StringBuilder result) {
        toMarkdown(result, "~");
    }

    @Override
    public void toBBCode(StringBuilder result) {
        toBBCode(result, "[s]", "[/s]");
    }
}
