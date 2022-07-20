package markup;

import java.util.List;

public class Paragraph extends AbstractElement {
    public Paragraph(List<InParagraph> list) {
        super(list);
    }

    public void toMarkdown(StringBuilder result) {
        toMarkdown(result, "");
    }

    public void toBBCode(StringBuilder result) {
        toBBCode(result, "", "");
    }

}
