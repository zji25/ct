package info.kgeorgiy.ja.yarunina.i18n;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.*;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Stream;

public class TextStatistics {
    private final Locale inputLocale;
    private final Locale outputLocale;
    private final String inputFileName;
    private final String input;
    private final Path outputPath;

    private final Collator collator;
    private final Map<Type, NumberFormat> numberFormats;
    private final List<DateFormat> dateFormats;
    private final Map<Type, AbstractTypeStatistics<?>> statistics;

    private static final Comparator<Number> numberComparator = Comparator.comparingDouble(Number::doubleValue);
    private static final String RESOURCE_BUNDLE_PATH = "info.kgeorgiy.ja.yarunina.i18n.TextStatisticsResourceBundle";


    public TextStatistics(
            final Locale inputLocale,
            final Locale outputLocale,
            final Path inputPath,
            final Path outputPath) throws IOException {
        final String language = outputLocale.getLanguage();
        if (language.isEmpty() || !language.equals("en") && !language.equals("ru")) {
            throw new IllegalArgumentException("output is only supported in english or russian");
        }
        this.inputLocale = inputLocale;
        this.outputLocale = outputLocale;
        this.inputFileName = inputPath.getFileName().toString();
        this.input = Files.readString(inputPath);
        this.outputPath = outputPath;
        this.collator = Collator.getInstance(inputLocale);
        this.numberFormats = Map.of(
                Type.NUMBER, NumberFormat.getNumberInstance(inputLocale),
                Type.MONEY, NumberFormat.getCurrencyInstance(inputLocale));
        this.dateFormats =
                Stream.of(DateFormat.DEFAULT, DateFormat.SHORT, DateFormat.MEDIUM, DateFormat.LONG, DateFormat.FULL)
                .map(x -> DateFormat.getDateInstance(x, inputLocale))
                .toList();
        final Tokens tokens = getTokens();
        this.statistics = Map.of(Type.SENTENCE, getSentenceStats(),
                Type.WORD, new TextTypeStatistics(tokens.words, new TreeSet<>(collator)),
                Type.NUMBER, new NumericTypeStatistics(tokens.numbers, new TreeSet<>(numberComparator)),
                Type.MONEY, new NumericTypeStatistics(tokens.money, new TreeSet<>(numberComparator)),
                Type.DATE, new DateTypeStatistics(tokens.dates, new TreeSet<>()));
    }

    public Map<Type, AbstractTypeStatistics<?>> getStatistics() {
        return statistics;
    }

    public void write() throws IOException {
        try (final BufferedWriter writer = Files.newBufferedWriter(outputPath, StandardCharsets.UTF_8)) {
            new StatisticsPrinter(
                    writer,
                    inputFileName,
                    ResourceBundle.getBundle(RESOURCE_BUNDLE_PATH, outputLocale),
                    numberFormats,
                    DateFormat.getDateInstance(DateFormat.DEFAULT, inputLocale),
                    this.getStatistics()
            ).printStatistics();
        }
    }

    private TextTypeStatistics getSentenceStats() {
        final List<String> sentences = new ArrayList<>();

        final BreakIterator sentenceIterator = BreakIterator.getSentenceInstance(inputLocale);
        sentenceIterator.setText(input);

        int start = sentenceIterator.first();
        int end = sentenceIterator.next();
        while (end != BreakIterator.DONE) {
            final String current = input.substring(start, end);
            if (Character.isLetterOrDigit(current.charAt(0))) {
                sentences.add(current);
            }
            start = end;
            end = sentenceIterator.next();
        }
        return new TextTypeStatistics(sentences, new TreeSet<>(collator));
    }

    private Tokens getTokens() {
        final List<String> words = new ArrayList<>();
        final List<Date> dates = new ArrayList<>();
        final Map<Type, List<Number>> numberTypes = Map.of(
                Type.NUMBER, new ArrayList<>(),
                Type.MONEY, new ArrayList<>());

        final BreakIterator wordIterator = BreakIterator.getWordInstance(inputLocale);
        wordIterator.setText(input);

        int start = wordIterator.first();
        int end = wordIterator.next();
        final ParsePosition position = new ParsePosition(0);
        while (end != BreakIterator.DONE) {
            if (foundDate(dates, dateFormats, start, position)
                    || foundNumberType(numberTypes::get, numberFormats, start, position)) {
                start = wordIterator.following(position.getIndex());
                end = wordIterator.next();
                continue;
            }
            final String currentWord = input.substring(start, end);
            if (Character.isLetter(currentWord.charAt(0))) {
                words.add(currentWord);
            }
            start = end;
            end = wordIterator.next();
        }
        return new Tokens(words, numberTypes.get(Type.NUMBER), numberTypes.get(Type.MONEY), dates);
    }

    private boolean foundDate(
            final List<Date> dates,
            final List<DateFormat> formats,
            final int start,
            final ParsePosition position) {
        for (final DateFormat format : formats) {
            if (tryParse(position, start, format, dates)) {
                return true;
            }
        }
        return false;
    }

    private boolean foundNumberType(
            final Function<Type, List<Number>> function,
            final Map<Type, NumberFormat> formats,
            final int start,
            final ParsePosition position) {
        for (final Map.Entry<Type, ? extends Format> entry : formats.entrySet()) {
            if (tryParse(position, start, entry.getValue(), function.apply(entry.getKey()))) {
                return true;
            }
        }
        return false;
    }

    @SuppressWarnings("unchecked")
    private <T> boolean tryParse(
            final ParsePosition position,
            final int index,
            final Format format,
            final List<T> elements) {
        position.setIndex(index);
        final Object parsed = format.parseObject(input, position);
        if (parsed == null) {
            return false;
        }
        elements.add((T) parsed);
        return true;
    }

    private record Tokens(List<String> words, List<Number> numbers, List<Number> money, List<Date> dates) {
    }

    public static void main(final String[] args) {
        ResourceBundle bundle;
        try {
            bundle = ResourceBundle.getBundle(RESOURCE_BUNDLE_PATH, Locale.getDefault());
        } catch (final MissingResourceException e) {
            bundle = ResourceBundle.getBundle(RESOURCE_BUNDLE_PATH, Locale.ENGLISH);
        }
        if (args == null || args.length != 4) {
            System.err.println(bundle.getString("usage"));
            return;
        }
        try {
            new TextStatistics(
                    Locale.forLanguageTag(args[0]),
                    Locale.forLanguageTag(args[1]),
                    Paths.get(args[2]),
                    Paths.get(args[3])
            ).write();
        } catch (final InvalidPathException e) {
            System.err.println(bundle.getString("file-not-found-error") + e.getMessage());
        } catch (final IllegalArgumentException e) {
            System.err.println(bundle.getString("path-error") + e.getMessage());
        } catch (final IOException e) {
            System.err.println(bundle.getString("io-error") + e.getMessage());
        }
    }
}
