package info.kgeorgiy.ja.yarunina.i18n;

import java.util.List;
import java.util.TreeSet;

public abstract class AbstractTypeStatistics<T> {
    protected int amount = 0;
    protected int uniqueAmount = 0;
    protected int sumLength = 0;

    protected T minLengthElement;
    protected T maxLengthElement;
    protected T minValueElement;
    protected T maxValueElement;
    protected T averageValueElement;

    public void getCommonStatistics(final List<T> elements, final TreeSet<T> set) {
        amount = elements.size();
        if (amount == 0) {
            return;
        }
        set.addAll(elements);
        uniqueAmount = set.size();
        minValueElement = set.first();
        maxValueElement = set.last();
    }

    public int getAmount() {
        return amount;
    }

    public int getUniqueAmount() {
        return uniqueAmount;
    }

    public double getAverageLength() {
        return ((double) sumLength) / amount;
    }

    public T getMinLengthElement() {
        return minLengthElement;
    }

    public T getMaxLengthElement() {
        return maxLengthElement;
    }

    public T getMinValueElement() {
        return minValueElement;
    }

    public T getMaxValueElement() {
        return maxValueElement;
    }

    public T getAverageValueElement() {
        return averageValueElement;
    }
}
