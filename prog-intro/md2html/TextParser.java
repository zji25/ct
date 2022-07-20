package md2html;

class TextParser {
    private static final String[] MD_TAGS, HTML_TAGS, NEW_MD_TAGS, NEW_HTML_TAGS;
    private static final int TAGS_AMOUNT, MAX_TAG_LENGTH, NEW_TAGS_AMOUNT;
    private final StringBuilder data;

    static {
        MD_TAGS = new String[]{"*", "_", "**", "__", "--", "`", "%", "''", "```"};
        NEW_MD_TAGS = new String[]{"<<", ">>", "}}", "{{"};
        HTML_TAGS = new String[]{"em", "em", "strong", "strong", "s", "code", "var", "q", "pre"};
        NEW_HTML_TAGS = new String[]{"ins", "del"};
        TAGS_AMOUNT = MD_TAGS.length; // 7
        NEW_TAGS_AMOUNT = NEW_MD_TAGS.length;
        MAX_TAG_LENGTH = 3;
    }

    TextParser(StringBuilder data) {
        this.data = data;
    }

    private int tag(String currentString) {
        int flag = -1;
        if (currentString.indexOf('\\') > -1) {
            currentString = currentString.substring(0, currentString.indexOf('\\'));
        }
        while (currentString.length() > 0 && flag == -1) {
            for (int i = 0; i < TAGS_AMOUNT + NEW_TAGS_AMOUNT; i++) {
                if (i < TAGS_AMOUNT && currentString.equals(MD_TAGS[i])
                        || i >= TAGS_AMOUNT && currentString.equals(NEW_MD_TAGS[i - TAGS_AMOUNT])) {
                    flag = i;
                    break;
                }
            }
            currentString = currentString.substring(0, currentString.length() - 1);
        }
        return flag;
    }

    public void toHtml(StringBuilder result) {
        String cur;
        int tag, tag_length;
        int[] indexes = new int[data.length()];
        int[] counters = new int[TAGS_AMOUNT + NEW_TAGS_AMOUNT];

        for (int i = 0; i < data.length(); i++) {
            if (data.charAt(i) == '\\') {
                i++;
                continue;
            }
            cur = data.substring(i, Math.min(i + MAX_TAG_LENGTH, data.length()));
            tag = tag(cur);
            if (tag > -1) {
                indexes[i] = tag + 1;
                if (tag < TAGS_AMOUNT) {
                    tag_length = MD_TAGS[tag].length();
                } else {
                    tag_length = NEW_MD_TAGS[tag - TAGS_AMOUNT].length();
                }
                for (int j = i + 1; j < i + tag_length; j++) {
                    indexes[j] = -1;
                }
                counters[tag]++;
                i += tag_length - 1;
            }
        }

        for (int i = 0; i < TAGS_AMOUNT; i++) {
            if (counters[i] % 2 != 0) {
                tag_length = MD_TAGS[i].length();
                counters[i]--;
                for (int j = data.length() - 1; j >= 0; j--) {
                    if (indexes[j] == i + 1) {
                        for (int k = j; k < j + tag_length; k++) {
                            indexes[k] = 0;
                        }
                        break;
                    }
                }
            }
        }

        boolean[] end = new boolean[TAGS_AMOUNT];
        boolean[] new_end = new boolean[NEW_TAGS_AMOUNT / 2];
        int k, j;

        for (int i = 0; i < data.length(); i++) {
            if (indexes[i] == 0) {
                new SpecialParser(data.charAt(i)).toHtml(result);
            } else if (0 < indexes[i] && indexes[i] <= TAGS_AMOUNT) {
                if (HTML_TAGS[indexes[i] - 1].equals("pre")) { // pre
                    j = 3;
                    result.append("<").append(HTML_TAGS[indexes[i] - 1]).append(">");
                    while (indexes[i + j] != TAGS_AMOUNT && (i + j) < data.length()) {
                        new SpecialParser(data.charAt(i + j)).toHtml(result);
                        j++;
                    }
                    result.append("</").append(HTML_TAGS[indexes[i] - 1]).append(">");
                    i += j;
                } else {  // old tags
                    if (end[indexes[i] - 1]) {
                        result.append("</").append(HTML_TAGS[indexes[i] - 1]).append(">");
                        end[indexes[i] - 1] = false;
                    } else {
                        result.append("<").append(HTML_TAGS[indexes[i] - 1]).append(">");
                        end[indexes[i] - 1] = true;
                    }
                }
            } else if (indexes[i] > TAGS_AMOUNT) {  // new tags
                k = indexes[i] - TAGS_AMOUNT - 1;
                if (new_end[k / 2] && k % 2 != 0) {
                    result.append("</").append(NEW_HTML_TAGS[k / 2]).append(">");
                    new_end[k / 2] = false;
                } else if ((!new_end[k / 2]) && k % 2 == 0) {
                    result.append("<").append(NEW_HTML_TAGS[k / 2]).append(">");
                    new_end[k / 2] = true;
                }
            }
        }
    }
}
