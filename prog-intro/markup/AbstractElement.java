package markup;

import java.util.List;

abstract class AbstractElement {
    private List<InParagraph> elements;

    AbstractElement(List<InParagraph> elements) {
        this.elements = elements;
    }

    void toMarkdown(StringBuilder result, String mark) {
        result.append(mark);
        for (InParagraph element : elements) {
            element.toMarkdown(result);
        }
        result.append(mark);
    }
    void toBBCode(StringBuilder result, String start, String end) {
        result.append(start);
        for (InParagraph element : elements) {
            element.toBBCode(result);
        }
        result.append(end);
    }
}

