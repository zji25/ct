package scanner;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.NoSuchElementException;

public class MyScanner {
    private final BufferedReader reader;
    private final StringBuilder builder = new StringBuilder();
    private int c = -1;
    private boolean saveMin;
    private boolean negative;

    public MyScanner(InputStream input) {
        this.reader = new BufferedReader(new InputStreamReader(input));
    }

    public MyScanner(String input) {
        this.reader = new BufferedReader(new StringReader(input));
    }

    public MyScanner(String path, String encoding) throws IOException {
        this.reader = new BufferedReader(new InputStreamReader(new FileInputStream(path), encoding));
    }

    public boolean hasNextLine() throws IOException {
        if (saveMin) {
            return true;
        } else {
            if (c == -1) {
                c = reader.read();
            }
            return c != -1;
        }
    }

    public String nextLine() throws IOException {
        char ch;
        String res;
        String cur;
        if (!saveMin) {
            if (c == 10) {
                res = Character.toString((char) c);
            } else if (c != -1) {
                cur = reader.readLine();
                if (cur != null) {
                    ch = (char) c;
                    res = ch + cur;
                } else {
                    res = Character.toString((char) c);
                }
            } else {
                res = reader.readLine();
            }
        } else {
            if (c == 10) {
                res = "-";
            } else if (c != -1) {
                cur = reader.readLine();
                if (cur != null) {
                    ch = (char) c;
                    res = "-" + ch + cur;
                } else {
                    res = "-" + (char) c;
                }
            } else {
                res = "-" + reader.readLine();
            }

            saveMin = false;
        }
        c = reader.read();
        return res;
    }

    public boolean hasNextInt() throws IOException {
        negative = false;
        if (c != -1 && c != 32 && c != 10) {
            if (Character.isDigit((char) c)) {
                return true;
            }

            if (Character.getType(c) == 20) {
                negative = true;
                c = reader.read();
                if (!Character.isDigit((char) c)) {
                    negative = false;
                    saveMin = true;
                }
            }
            return negative;
        }
        c = reader.read();
        while (c != -1) {
            if (c != -1 && c != 32 && c != 10) {
                if (Character.isDigit((char) c)) {
                    return true;
                }
                if (Character.getType(c) == 20) {
                    negative = true;
                    c = reader.read();
                    if (!Character.isDigit((char) c)) {
                        negative = false;
                        saveMin = true;
                    }
                }
                return negative;
            }
            c = reader.read();
        }
        return false;
    }

    public int nextInt() throws IOException {
        if (c == -1) {
            c = reader.read();
        }
        if (negative) {
            builder.append('-');
        }
        for(negative = false; Character.isDigit((char) c); c = reader.read()) {
            builder.append((char) c);
        }
        if (!builder.toString().equals("-") && !builder.isEmpty()) {
            int res = Integer.parseInt(builder.toString());
            builder.setLength(0);
            return res;
        } else {
            throw new NoSuchElementException(":( no next int found");
        }
    }

    public boolean hasNextAbcInt() throws IOException {
        negative = false;
        if (c != -1 && c != 32 && c != 10) {
            if (Character.isLetter((char) c)) {
                return true;
            }
            if (Character.getType(c) == 20) {
                negative = true;
                c = this.reader.read();
                if (!Character.isLetter((char) c)) {
                    negative = false;
                    saveMin = true;
                }
            }
            return negative;
        }
        c = reader.read();
        while (c != -1) {
            if (c != -1 && c != 32 && c != 10) {
                if (Character.isLetter((char) c)) {
                    return true;
                }
                if (Character.getType(c) == 20) {
                    negative = true;
                    c = this.reader.read();
                    if (!Character.isLetter((char) c)) {
                        negative = false;
                        saveMin = true;
                    }
                }
                return negative;
            }
            c = reader.read();
        }
        return false;
    }

    public int nextAbcInt() throws IOException {
        if (c == -1) {
            c = reader.read();
        }
        if (negative) {
            builder.append('-');
        }
        for(negative = false; Character.isLetter((char) c); c = reader.read()) {
            builder.append((char) c - 97);
        }
        if (!builder.toString().equals("-") && !builder.isEmpty()) {
            int res = Integer.parseInt(builder.toString());
            builder.setLength(0);
            return res;
        } else {
            throw new NoSuchElementException(":( no next abcInt found");
        }
    }

    public int read() throws IOException {
        return reader.read();
    }
    public void close() throws IOException {
        reader.close();
    }
}
