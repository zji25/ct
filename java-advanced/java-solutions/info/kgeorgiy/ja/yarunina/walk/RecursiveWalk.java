package info.kgeorgiy.ja.yarunina.walk;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.*;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class RecursiveWalk {
    public static void main(final String[] args) {
        if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
            System.err.println("enter names of input and output files");
            return;
        }

        final Path inputFilePath;
        final Path outputFilePath;
        try {
            inputFilePath = convertPath(args[0], "input");
            outputFilePath = convertPath(args[1], "output");
        } catch (final InvalidPathException ignored) {
            return;
        }

        try {
            final Path outputParentPath = outputFilePath.getParent();
            if (outputParentPath != null) {
                Files.createDirectories(outputParentPath);
            }
        } catch (final IOException ignored) {}

        try (final BufferedReader reader = Files.newBufferedReader(inputFilePath)) {
            try (final BufferedWriter writer = Files.newBufferedWriter(outputFilePath)) {
                final MessageDigest digest;
                try {
                    digest = MessageDigest.getInstance("SHA-256");
                } catch (final NoSuchAlgorithmException e) {
                    throw new AssertionError("couldn't find hash algorithm's implementation: " + e.getMessage());
                }
                final HashFileVisitor visitor = new HashFileVisitor(writer, digest);
                while (true) {
                    final String line;
                    try {
                        line = reader.readLine();
                    } catch (final IOException e) {
                        System.err.println("error while reading input file: " + e.getMessage());
                        return;
                    }
                    if (line == null) {
                        break;
                    }
                    try {
                        try {
                            Files.walkFileTree(Paths.get(line), visitor);
                        } catch (final InvalidPathException e) {
                            visitor.getHashWriter().writeHash(line);
                        }
                    } catch (final IOException e) {
                        System.err.println("error while writing to output file: " + e.getMessage());
                        return;
                    }
                }
            } catch (final IOException e) {
                System.err.println("can't open output file: " + e.getMessage());
            }
        } catch (final IOException e) {
            System.err.println("can't open input file: " + e.getMessage());
        }
    }

    private static Path convertPath(final String path, final String name) {
        try {
            return Paths.get(path);
        } catch (final InvalidPathException e) {
            System.err.println(name + " file name can't be converted to a path: " + e.getMessage());
            throw e;
        }
    }
}
