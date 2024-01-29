package test.calc.generated;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import java.text.ParseException;

public class ParserTest {
    private final Parser parser = new Parser();

    @ParameterizedTest @CsvSource(value = {
    "2.4; 2.4",
	"-17+8; -9",
	"1 * 2/3; 0.66666",
	"2/3/2/3; 0.11111",
	"2^3^2; 512",
	"4!; 24",
	"4!!; 8",
	"4!!!; 40320",
	"1-2-3; -4",
	"[4.4]; 4",
	"{4.4}; 0.4",
	"{[5.2]}; 0"
    }, delimiter = ';')
    public void test(final String input, double expected) {
        try {
            Assertions.assertEquals(expected, parser.parse(input).val, 0.0001);
        } catch (final ParseException e) {
            throw new RuntimeException(e);
        }
    }
}
