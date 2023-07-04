package info.kgeorgiy.ja.yarunina.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.IntStream;
import java.util.stream.Stream;

@SuppressWarnings("unused")
public class ParallelMapperImpl implements ParallelMapper {
    private final List<Thread> threads;
    private final SynchronizedQueue<RunnableWrapper<?>> queue;
    private boolean isOpen;

    public ParallelMapperImpl(final int threadNumber) {
        if (threadNumber < 1) {
            throw new IllegalArgumentException("number of threads can't be less than 1");
        }
        this.isOpen = true;
        this.queue = new SynchronizedQueue<>();
        final Runnable task = () -> {
            try {
                while (!Thread.currentThread().isInterrupted()) {
                    this.queue.take().runnable.run();
                }
            } catch (final InterruptedException ignored) {
            }
        };
        this.threads = Stream.generate(() -> new Thread(task)).limit(threadNumber).toList();
        this.threads.forEach(Thread::start);
    }

    @Override
    public <T, R> List<R> map(
            final Function<? super T, ? extends R> function,
            final List<? extends T> args
    ) throws InterruptedException {
        final Results<R> results = new Results<>(args.size());
        IntStream.range(0, args.size())
                .<Runnable>mapToObj(i -> () -> {
                    try {
                        results.set(i, function.apply(args.get(i)));
                    } catch (final RuntimeException e) {
                        results.set(e);
                    }
                    // :NOTE: putAll
                }).forEach(runnable -> queue.put(new RunnableWrapper<>(runnable, results)));
        results.waitForTerminated();
        return results.getResultsList();
    }

    @Override
    public void close() {
        this.isOpen = false;
        final Set<Results<?>> notified = new HashSet<>();
        this.queue.forEachSync(x -> {
            if (notified.add(x.results)) {
                x.results.notifySync();
            }
        });
        this.threads.forEach(Thread::interrupt);
        this.threads.forEach(thread -> {
            while (true) {
                try {
                    thread.join();
                    break;
                } catch (final InterruptedException e) {
                    thread.interrupt();
                }
            }
        });
    }

    private record RunnableWrapper<R>(Runnable runnable, Results<R> results) {}

    private static class SynchronizedQueue<T> {
        private final Queue<T> queue = new ArrayDeque<>();

        public synchronized T take() throws InterruptedException {
            while (queue.isEmpty()) {
                wait();
            }
            return queue.poll();
        }

        public synchronized void put(final T element) {
            queue.add(element);
            notify();
        }

        public synchronized void forEachSync(final Consumer<? super T> action) {
            queue.forEach(action);
        }
    }

    private class Results<R> {
        private final List<R> resultsList;
        private int incompleteTasks;
        private RuntimeException exception;

        public Results(final int taskNumber) {
            this.incompleteTasks = taskNumber;
            this.exception = null;
            this.resultsList = new ArrayList<>(Collections.nCopies(taskNumber, null));
        }

        public List<R> getResultsList() {
            return resultsList;
        }

        public synchronized void set(final int index, final R element) {
            resultsList.set(index, element);
            decrement();
        }

        public synchronized void set(final RuntimeException exception) {
            if (this.exception == null) {
                this.exception = exception;
            } else {
                this.exception.addSuppressed(exception);
            }
            decrement();
        }

        public synchronized void waitForTerminated() throws InterruptedException {
            while (incompleteTasks > 0 && ParallelMapperImpl.this.isOpen) {
                wait();
            }
            if (!ParallelMapperImpl.this.isOpen) {
                throw new IllegalStateException("couldn't complete map because ParallelMapper was closed");
            }
            if (exception != null) {
                throw exception;
            }
        }

        public synchronized void notifySync() {
            notify();
        }

        private void decrement() {
            if (--incompleteTasks == 0) {
                notify();
            }
        }
    }
}
