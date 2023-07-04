package info.kgeorgiy.ja.yarunina.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ListIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

@SuppressWarnings("unused")
public class IterativeParallelism implements ListIP {
    private final ParallelMapper mapper;

    public IterativeParallelism() {
        this(null);
    }

    public IterativeParallelism(final ParallelMapper mapper) {
        this.mapper = mapper;
    }

    @Override
    public <T> T minimum(
            final int threads,
            final List<? extends T> values,
            final Comparator<? super T> comparator
    ) throws InterruptedException {
        final Function<List<? extends T>, ? extends T> min = list -> Collections.min(list, comparator);
        return parallelize(threads, values, min, min);
    }

    @Override
    public <T> T maximum(
            final int threads,
            final List<? extends T> values,
            final Comparator<? super T> comparator
    ) throws InterruptedException {
        return minimum(threads, values, comparator.reversed());
    }

    @Override
    public <T> boolean any(
            final int threads,
            final List<? extends T> values,
            final Predicate<? super T> predicate
    ) throws InterruptedException {
        return parallelize(threads, values,
                list -> list.stream().anyMatch(predicate),
                list -> list.contains(true));
    }

    @Override
    public <T> boolean all(
            final int threads,
            final List<? extends T> values,
            final Predicate<? super T> predicate
    ) throws InterruptedException {
        return !any(threads, values, predicate.negate());
    }

    @Override
    public String join(
            final int threads,
            final List<?> values
    ) throws InterruptedException {
        return parallelize(threads, values,
                list -> list.stream()
                        .map(Objects::toString)
                        .collect(Collectors.joining()),
                list -> String.join("", list)
        );
    }

    @Override
    public <T> List<T> filter(
            final int threads,
            final List<? extends T> values,
            final Predicate<? super T> predicate
    ) throws InterruptedException {
        return parallelizeAndConcat(threads, values, stream -> stream.filter(predicate));
    }

    @Override
    public <T, U> List<U> map(
            final int threads,
            final List<? extends T> values,
            final Function<? super T, ? extends U> f
    ) throws InterruptedException {
        return parallelizeAndConcat(threads, values, stream -> stream.map(f));
    }

    @Override
    public <T> int count(
            final int threads,
            final List<? extends T> values,
            final Predicate<? super T> predicate
    ) throws InterruptedException {
        return parallelize(threads, values,
                list -> (int) list.stream().filter(predicate).count(),
                list -> list.stream().mapToInt(Integer::intValue).sum());
    }

    private <T, R, F> F parallelize(
            int threadNumber,
            final List<? extends T> values,
            final Function<List<? extends T>, ? extends R> subFunction,
            final Function<List<? extends R>, F> function
    ) throws InterruptedException {
        if (threadNumber < 1) {
            throw new IllegalArgumentException("number of threads can't be less than 1");
        }
        if (values.isEmpty()) {
            return function.apply(List.of());
        }

        threadNumber = Math.min(threadNumber, values.size());
        final int subListSize = values.size() / threadNumber;
        final int mod = values.size() % threadNumber;

        final List<List<? extends T>> subLists = new ArrayList<>();
        int end = 0;
        for (int i = 0; i < threadNumber; i++) {
            final int start = end;
            end += subListSize + (i < mod ? 1 : 0);
            subLists.add(values.subList(start, end));
        }

        return function.apply(this.mapper != null
                ? this.mapper.map(subFunction, subLists)
                : map(threadNumber, subFunction, subLists));
    }

    private static <T, R> List<R> map(
            final int threadNumber,
            final Function<List<? extends T>, ? extends R> subFunction,
            final List<List<? extends T>> subLists
    ) throws InterruptedException {
        final List<R> results = new ArrayList<>(Collections.nCopies(threadNumber, null));
        final RuntimeExceptionWrapper runtimeException = new RuntimeExceptionWrapper();
        final List<Thread> threads = IntStream.range(0, threadNumber)
                .mapToObj(i -> new Thread(() -> {
                    try {
                        results.set(i, subFunction.apply(subLists.get(i)));
                    } catch (final RuntimeException e) {
                        runtimeException.add(e);
                    }
                })).peek(Thread::start)
                .toList();

        InterruptedException interruptedException = null;
        for (final Thread thread : threads) {
            while (true) {
                try {
                    thread.join();
                    break;
                } catch (final InterruptedException e) {
                    if (interruptedException == null) {
                        interruptedException = e;
                        threads.forEach(Thread::interrupt);
                    } else {
                        interruptedException.addSuppressed(e);
                        thread.interrupt();
                    }
                }
            }
        }

        if (runtimeException.get() != null) {
            throw runtimeException.get();
        }
        if (interruptedException != null) {
            throw interruptedException;
        }
        return results;
    }

    private <T, U> List<U> parallelizeAndConcat(
            final int threads,
            final List<? extends T> values,
            final Function<Stream<? extends T>, Stream<? extends U>> subFunction
    ) throws InterruptedException {
        return parallelize(threads, values,
                list -> subFunction.apply(list.stream()).toList(),
                list -> list.stream()
                        .flatMap(Collection::stream)
                        .collect(Collectors.toList()));
    }

    private static class RuntimeExceptionWrapper {
        private RuntimeException exception;

        public RuntimeExceptionWrapper() {
            this.exception = null;
        }

        public synchronized void add(final RuntimeException e) {
            if (exception == null) {
                exception = e;
            } else {
                exception.addSuppressed(e);
            }
        }

        public RuntimeException get() {
            return exception;
        }
    }
}
