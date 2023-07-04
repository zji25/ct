package info.kgeorgiy.ja.yarunina.i18n.tests;

import info.kgeorgiy.ja.yarunina.i18n.AbstractTypeStatistics;
import info.kgeorgiy.ja.yarunina.i18n.TextStatistics;
import info.kgeorgiy.ja.yarunina.i18n.Type;
import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

public class TextStatisticsTest {
    private static final String SAMPLES_PATH = "java-solutions/info/kgeorgiy/ja/yarunina/i18n/tests/samples/";
    private static final String BUNDLE_PATH = "info.kgeorgiy.ja.yarunina.i18n.TextStatisticsResourceBundle";
    private static final Path OUTPUT_FILE_PATH = Paths.get(SAMPLES_PATH, "output.txt");
    private static final List<String> UNTRACKED_PROPERTIES = List.of("usage", "header", "unique", "error");

    private static final Locale LOCALE_EN = Locale.ENGLISH;
    private static final Locale LOCALE_RU = Locale.forLanguageTag("ru");

    private final List<String> propertyKeys;

    public TextStatisticsTest() {
        final ResourceBundle rb = ResourceBundle.getBundle(BUNDLE_PATH);
        propertyKeys = rb.keySet().stream().filter(x -> {
            for (final String untracked : UNTRACKED_PROPERTIES) {
                if (x.contains(untracked)) {
                    return false;
                }
            }
            return true;
        }).toList();
    }

    @Test
    public void test1_en() {
        test_text("test1_en", "en_US", 10, 57);
    }

    @Test
    public void test2_ru() {
        test_text("test2_ru", "ru_RU", 2, 36);
    }

    @Test
    public void test3_different_dates_en() {
        test_type("test3_different_dates_en", "en_US", Type.DATE,
                9, 3,
                new GregorianCalendar(2012, Calendar.MARCH, 25, 8, 20, 0).getTime(),
                new GregorianCalendar(1999, Calendar.MAY, 2).getTime(),
                new GregorianCalendar(2023, Calendar.APRIL, 1).getTime());
    }

    @Test
    public void test4_different_dates_ar() {
        test_type("test4_different_dates_ar", "ar", Type.DATE,
                12, 3,
                new GregorianCalendar(1903, Calendar.FEBRUARY, 4, 7, 30, 17).getTime(),
                new GregorianCalendar(1764, Calendar.OCTOBER, 11).getTime(),
                new GregorianCalendar(2023, Calendar.JULY, 1).getTime());
    }

    @Test
    public void test5_money_ru() {
        test_type("test5_money_ru", "ru_RU", Type.MONEY,
                10, 8, 16245.0, 150L, 100000L);
    }

    @Test
    public void test6_money_fr() {
        test_type("test6_money_fr", "fr_FR", Type.MONEY,
                8, 6, 46.875, 10L, 140L);
    }

    @Test
    public void test7_numbers_de() {
        test_type("test7_numbers_de", "de_DE", Type.NUMBER, 5, 5, 2.4596777941669397E9, -47289923L, 12345678912L);
        test_type("test7_numbers_de", "de_DE", Type.DATE, 3, 3,
                new GregorianCalendar(1903, Calendar.FEBRUARY, 4, 7, 30, 17).getTime(),
                new GregorianCalendar(1764, Calendar.OCTOBER, 11).getTime(),
                new GregorianCalendar(2023, Calendar.JULY, 1).getTime());
        test_type("test7_numbers_de", "de_DE", Type.MONEY, 0, 0, null, null, null);
    }

    @Test
    public void test8_text_count_zh() {
        test_text("test8_text_count_zh", "zh_CN", 3, 3);
    }

    @Test
    public void test9_compare_output() {
        final Locale locale = Locale.forLanguageTag("en_US");
        final ResourceBundle english = ResourceBundle.getBundle(BUNDLE_PATH, LOCALE_EN);
        final ResourceBundle russian = ResourceBundle.getBundle(BUNDLE_PATH, LOCALE_RU);

        final Map<String, String> englishData = new HashMap<>();
        try {
            write("test1_en", locale, LOCALE_EN);
            final String englishOutput = Files.readString(OUTPUT_FILE_PATH);
            propertyKeys.forEach(property ->
                    englishData.put(property, getStringByProperty(english, englishOutput, property)));
            write("test1_en", locale, LOCALE_RU);
            final String russianOutput = Files.readString(OUTPUT_FILE_PATH);
            propertyKeys.forEach(property ->
                    Assert.assertEquals(
                            "output in different locales mismatches",
                            englishData.get(property),
                            getStringByProperty(russian, russianOutput, property)));
        } catch (final IOException e) {
            throw new AssertionError(e);
        }
    }

    private static String getStringByProperty(final ResourceBundle bundle, final String output, final String property) {
        final String propertyValue = bundle.getString(property);
        final int index = output.indexOf(propertyValue) + propertyValue.length() + 1;
        return output.substring(index, output.indexOf("\n", index));
    }


    private <T> void test_type(
            final String file,
            final String locale,
            final Type type,
            final int amount,
            final int unique,
            final T average,
            final T min,
            final T max) {
        final AbstractTypeStatistics<?> result =
                getTextStatistics(Locale.forLanguageTag(locale), Paths.get(SAMPLES_PATH + file)).get(type);
        Assert.assertEquals("wrong amount", amount, result.getAmount());
        Assert.assertEquals("wrong unique amount", unique, result.getUniqueAmount());
        final String wrongAverageMessage = "wrong average";
        if (average instanceof Number) {
            Assert.assertEquals(wrongAverageMessage,
                    ((Number) average).doubleValue(),
                    ((Number) result.getAverageValueElement()).doubleValue(),
                    1e-5);
        } else {
            Assert.assertEquals(wrongAverageMessage, average, result.getAverageValueElement());
        }
        Assert.assertEquals("wrong min", min, result.getMinValueElement());
        Assert.assertEquals("wrong max", max, result.getMaxValueElement());
    }

    private void test_text(
            final String file,
            final String locale,
            final int sentenceCount,
            final int wordCount) {
        final Map<Type, AbstractTypeStatistics<?>> textStatistics = getTextStatistics(
                Locale.forLanguageTag(locale), Paths.get(SAMPLES_PATH + file));
        Assert.assertEquals("wrong sentence amount", sentenceCount,
                textStatistics.get(Type.SENTENCE).getAmount());
        Assert.assertEquals("wrong word amount", wordCount,
                textStatistics.get(Type.WORD).getAmount());
    }

    private Map<Type, AbstractTypeStatistics<?>> getTextStatistics(
            final Locale inputLocale,
            final Path inputFile) {
        return getStatistics(inputLocale, LOCALE_EN, inputFile).getStatistics();
    }

    private void write(
            final String inputFile,
            final Locale inputLocale,
            final Locale outputLocale) {
        try {
            getStatistics(inputLocale, outputLocale, Paths.get(SAMPLES_PATH + inputFile)).write();
        } catch (final IOException e) {
            throw new AssertionError("error while writing", e);
        }
    }

    private TextStatistics getStatistics(final Locale inputLocale,
                               final Locale outputLocale,
                               final Path inputFile) {
        try {
            return new TextStatistics(inputLocale, outputLocale, inputFile, OUTPUT_FILE_PATH);
        } catch (final IOException e) {
            throw new AssertionError("error in a constructor", e);
        }
    }
}
