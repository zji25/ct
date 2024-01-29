package grammar;

public final class Common {
    public static String removeBrackets(final String s) { return removeBrackets(s, 1); }
    public static String removeBrackets(final String s, int i) {
        return s == null || s.length() < i*2 ? "" : s.substring(i, s.length() - i).trim();
    }
    public static String firstUpper(final String s) {
        return (s.length() <= 1) ? s.toUpperCase() : s.substring(0, 1).toUpperCase() + s.substring(1);
    }
}
