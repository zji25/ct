package info.kgeorgiy.ja.yarunina.i18n;

import java.io.BufferedWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.util.Map;
import java.util.ResourceBundle;

@SuppressWarnings("unused")
record StatisticsPrinter(
        BufferedWriter writer,
        String analyzedFile,
        ResourceBundle bundle,
        Map<Type, NumberFormat> numberFormats,
        DateFormat dateFormat,
        Map<Type, AbstractTypeStatistics<?>> typeStatisticsMap) {
    private static final int MAX_TEXT_LENGTH = 50;
    private static final String TEXT_FORMAT = "%1$." + MAX_TEXT_LENGTH + "s...";

    public void printStatistics() throws IOException {
        printMainHeader();
        printSummary();
        printTextStatistics(Type.SENTENCE);
        printTextStatistics(Type.WORD);
        printOtherStatistics(Type.NUMBER);
        printOtherStatistics(Type.MONEY);
        printOtherStatistics(Type.DATE);
    }

    private void printMainHeader() throws IOException {
        writer.write(String.format("%s \"%s\"\n", getHeader("main"), analyzedFile));
    }

    private void printSummary() throws IOException {
        writer.write(String.format("%s\n%s%s%s%s%s",
                getHeader("summary"),
                getAmountString(Type.SENTENCE, false),
                getAmountString(Type.WORD, false),
                getAmountString(Type.NUMBER, false),
                getAmountString(Type.MONEY, false),
                getAmountString(Type.DATE, false)));
    }

    private void printTextStatistics(final Type type) throws IOException {
        writer.write(String.format("%s\n%s%s",
                getHeader(type), getAmountString(type, true), getConcreteStats(type)));
    }

    private void printOtherStatistics(final Type type) throws IOException {
        writer.write(String.format("%s\n%s%s",
                getHeader(type), getAmountString(type, false), getConcreteStats(type)));
    }


    public String getAmountString(final Type type, final boolean includeUnique) {
        final int uniqueAmount = typeStatisticsMap.get(type).getUniqueAmount();
        return String.format("\t%s %d%s.\n",
                bundle.getString(type.toString() + "-number"),
                typeStatisticsMap.get(type).getAmount(),
                (includeUnique ?
                        String.format(" (%d %s)",
                                uniqueAmount, bundle.getString("unique" + (uniqueAmount == 1 ? "-one" : "")))
                        : ""));
    }

    private String getConcreteStats(final Type type) {
        return typeStatisticsMap.get(type).getAmount() == 0 ? "" :
                String.format("%s%s\n", getMinMaxElementString(type),
                        type.isText() ? getLengthStatistics(type) : getAverage(type));
    }


    private String getHeader(final String prefix) {
        return bundle.getString(prefix + "-header");
    }

    private String getHeader(final Type type) {
        return getHeader(type.toString());
    }


    private String getMinMaxElementString(final Type type) {
        final AbstractTypeStatistics<?> current = typeStatisticsMap.get(type);
        return String.format("\t%s\n\t%s\n",
                getMinMax(type, "min-", getFormatted(type, current.getMinValueElement())),
                getMinMax(type, "max-", getFormatted(type, current.getMaxValueElement())));
    }

    private String getFormatted(final Type type, final Object element) {
        if (type.isText()) {
            final String current = String.valueOf(element);
            return String.format(current.length() > MAX_TEXT_LENGTH ? TEXT_FORMAT : "%s", current);
        }
        if (type == Type.DATE) {
            return dateFormat.format(element);
        }
        return numberFormats.get(type).format(element);
    }

    private String getMinMax(final Type type, final String prefix, final String element) {
        return String.format(type.isText() ? "%s \"%s\"." : "%s %s.", bundle.getString(prefix + type), element);
    }

    private String getMinMaxLength(final Type type, final String prefix, final String element) {
        return String.format("%s %d (\"%s\").\n",
                bundle.getString(prefix + "length-"+  type.toString()), element.length(), element);
    }

    private String getLengthStatistics(final Type type) {
        final AbstractTypeStatistics<?> current = typeStatisticsMap.get(type);
        return String.format("\t%s\t%s\t%s %f.",
                getMinMaxLength(type, "min-", getFormatted(type, current.getMinLengthElement())),
                getMinMaxLength(type, "max-", getFormatted(type, current.getMaxLengthElement())),
                bundle.getString("average-length-" + type),
                current.getAverageLength()
        );
    }

    private String getAverage(final Type type) {
        return String.format("\t%s %s.",
                bundle.getString("average-" + type.toString()),
                getFormatted(type, typeStatisticsMap.get(type).getAverageValueElement()));
    }
}
