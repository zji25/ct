package info.kgeorgiy.ja.yarunina.i18n;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.TreeSet;

public class TextTypeStatistics extends AbstractTypeStatistics<String> {
    final Comparator<String> lengthComparator = Comparator.comparingInt(String::length);

    public TextTypeStatistics(final List<String> elements, final TreeSet<String> set) {
        getCommonStatistics(elements, set);
        if (amount == 0) {
            return;
        }
        minLengthElement = Collections.min(elements, lengthComparator);
        maxLengthElement = Collections.max(elements, lengthComparator);
        sumLength = elements.stream().mapToInt(String::length).sum();
    }
}
