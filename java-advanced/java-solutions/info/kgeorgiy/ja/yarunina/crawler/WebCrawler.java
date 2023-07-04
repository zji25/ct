package info.kgeorgiy.ja.yarunina.crawler;

import info.kgeorgiy.java.advanced.crawler.*;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.*;
import java.util.concurrent.*;
import java.util.stream.IntStream;


public class WebCrawler implements Crawler {
    private static final int DEFAULT_DEPTH = 1;
    private static final int DEFAULT_DOWNLOADERS = 1;
    private static final int DEFAULT_EXTRACTORS = 1;
    private static final int DEFAULT_PER_HOST = Integer.MAX_VALUE;

    private final Map<String, Semaphore> hostSemaphores = new ConcurrentHashMap<>();

    private final Downloader downloader;
    private final ExecutorService downloaders;
    private final ExecutorService extractors;
    private final int perHost;

    public WebCrawler(
            final Downloader downloader,
            final int downloaders,
            final int extractors,
            final int perHost) {
        this.downloader = downloader;
        this.perHost = perHost;
        this.downloaders = Executors.newFixedThreadPool(downloaders);
        this.extractors = Executors.newFixedThreadPool(extractors);
    }

    @Override
    public Result download(final String url, final int depth) {
        return new RecursiveDownloader().getResult(url, depth);
    }

    @Override
    public void close() {
        this.extractors.close();
        this.downloaders.close();
    }

    private class RecursiveDownloader {
        private final Set<String> uniqueURLs = ConcurrentHashMap.newKeySet();
        private final Set<String> toProcess = ConcurrentHashMap.newKeySet();
        private final Map<String, IOException> errors = new ConcurrentHashMap<>();
        private final Phaser phaser = new Phaser(1);

        public Result getResult(final String currentURL, final int depth) {
            final List<String> toDownload = new ArrayList<>();
            toDownload.add(currentURL);
            IntStream.range(0, depth).forEach(i -> {
                toDownload.forEach(URL -> download(URL, depth - i));
                toDownload.clear();
                this.phaser.arriveAndAwaitAdvance();
                toDownload.addAll(this.toProcess);
                this.toProcess.clear();
            });
            this.uniqueURLs.removeAll(this.errors.keySet());
            return new Result(this.uniqueURLs.stream().toList(), this.errors);
        }

        private void download(final String currentURL, final int depth) {
            if (!this.uniqueURLs.add(currentURL)) {
                return;
            }
            try {
                final Semaphore semaphore = hostSemaphores.computeIfAbsent(
                        URLUtils.getHost(currentURL),
                        x -> new Semaphore(perHost, true));
                semaphore.acquire();
                this.phaser.register();
                downloaders.submit(() -> {
                    try {
                        final Document document = downloader.download(currentURL);
                        if (depth == 1) {
                            return;
                        }
                        this.phaser.register();
                        extractors.submit(() -> {
                            try {
                                this.toProcess.addAll(document.extractLinks());
                            } catch (final IOException ignored) {
                            } finally {
                                this.phaser.arriveAndDeregister();
                            }
                        });
                    } catch (final IOException e) {
                        this.errors.put(currentURL, e);
                    } finally {
                        this.phaser.arriveAndDeregister();
                        semaphore.release();
                    }
                });
            } catch (final MalformedURLException e) {
                this.errors.put(currentURL, e);
            } catch (final InterruptedException ignored) {
            }
        }
    }

    private static int getIntArgument(
            final String[] args,
            final int index,
            final int defaultValue) {
        return (args.length <= index) ? defaultValue : Integer.parseInt(args[index]);
    }

    public static void main(final String[] args) {
        if (args == null || args.length < 1) {
            System.err.println("usage:\n\turl [depth [downloads [extractors [perHost]]]]");
            return;
        }
        try {
            final CachingDownloader downloader = new CachingDownloader(0);
            try (final WebCrawler crawler = new WebCrawler(
                    downloader,
                    getIntArgument(args, 2, DEFAULT_DOWNLOADERS),
                    getIntArgument(args, 3, DEFAULT_EXTRACTORS),
                    getIntArgument(args, 4, DEFAULT_PER_HOST))) {
                final Result result = crawler.download(args[0], getIntArgument(args, 1, DEFAULT_DEPTH));
                System.out.println("downloaded successfully:");
                result.getDownloaded().forEach(System.out::println);
                final Map<String, IOException> errors = result.getErrors();
                if (!errors.isEmpty()) {
                    System.out.println(errors.size() + " errors :(\noccurred at:");
                    errors.forEach((url, exception) -> System.out.println(url + "\n\t" + exception.getMessage()));
                }
            } catch (final NumberFormatException e) {
                System.err.println("couldn't parse integer argument:\n\t" + e.getMessage());
            }
        } catch (final IOException e) {
            System.err.println("couldn't create a downloader:\n\t" + e.getMessage());
        }
    }
}
