package info.kgeorgiy.ja.yarunina.i18n;

import java.math.BigInteger;
import java.util.Date;
import java.util.List;
import java.util.TreeSet;

public class DateTypeStatistics extends AbstractTypeStatistics<Date> {
    public DateTypeStatistics(final List<Date> elements, final TreeSet<Date> set) {
        getCommonStatistics(elements, set);
        if (amount == 0) {
            return;
        }
        BigInteger total = BigInteger.ZERO;
        for (final Date date : elements) {
            total = total.add(BigInteger.valueOf(date.getTime()));
        }
        averageValueElement = new Date(total.divide(BigInteger.valueOf(amount)).longValue());
    }
}
