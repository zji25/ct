package info.kgeorgiy.ja.yarunina.i18n;

import java.util.List;
import java.util.TreeSet;

public class NumericTypeStatistics extends AbstractTypeStatistics<Number> {
    public NumericTypeStatistics(final List<Number> elements, final TreeSet<Number> set) {
        getCommonStatistics(elements, set);
        if (amount == 0) {
            return;
        }
        averageValueElement = elements.stream().mapToDouble(Number::doubleValue).sum() / amount;
    }
}
