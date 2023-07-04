package info.kgeorgiy.ja.yarunina.walk;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.MessageDigest;
import java.util.HexFormat;

public class HashFileVisitor extends SimpleFileVisitor<Path> {
    private final HashWriter hashWriter;
    private final MessageDigest digest;
    private final byte[] buffer = new byte[1 << 16];

    public HashFileVisitor(final BufferedWriter writer, final MessageDigest digest) {
        this.digest = digest;
        this.hashWriter = new HashWriter(writer, digest.digest().length * 2);
    }

    @Override
    public FileVisitResult visitFile(final Path path, final BasicFileAttributes attrs) throws IOException {
        hashWriter.writeHash(String.valueOf(path), hash(path));
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult visitFileFailed(final Path path, final IOException exception) throws IOException {
        hashWriter.writeHash(String.valueOf(path));
        return FileVisitResult.CONTINUE;
    }

    public HashWriter getHashWriter() {
        return hashWriter;
    }

    private byte[] hash(final Path path) {
        digest.reset();
        try (final InputStream stream = Files.newInputStream(path)) {
            int c;
            while ((c = stream.read(buffer)) >= 0) {
                digest.update(buffer, 0, c);
            }
            return digest.digest();
        } catch (final IOException e) {
            System.err.println("error while reading a file: " + e.getMessage());
            return null;
        }
    }


    public static class HashWriter {
        private final BufferedWriter writer;
        private final String zeroHashFormat;

        public HashWriter(final BufferedWriter writer, final int hashLength) {
            this.writer = writer;
            this.zeroHashFormat = "%0" + hashLength + "x %s";
        }

        public void writeHash(final String path, final byte[] hash) throws IOException {
            writer.write(hash == null
                    ? String.format(zeroHashFormat, 0, path)
                    : String.format("%s %s", HexFormat.of().formatHex(hash), path));
            writer.newLine();
        }

        public void writeHash(final String path) throws IOException {
            writeHash(path, null);
        }
    }
}
