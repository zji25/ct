package info.kgeorgiy.ja.yarunina.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.SocketException;
import java.nio.ByteBuffer;
import java.nio.channels.CancelledKeyException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static info.kgeorgiy.ja.yarunina.hello.HelloUDPUtil.*;

public class HelloUDPNonblockingServer implements HelloServer {
    private Selector selector;
    private DatagramChannel channel;
    private ExecutorService service;
    private Queue<ResponseWrapper> responses;
    private ExecutorService mainService;
    private int bufferSize;

    @Override
    public void start(final int port, final int threads) {
        if (this.selector != null && this.selector.isOpen()
                || this.channel != null && this.channel.isOpen()
                || this.service != null && !this.service.isTerminated()) {
            System.err.println("please close the server before restarting");
            return;
        }
        try {
            this.selector = Selector.open();

            this.channel = DatagramChannel.open();
            this.channel.configureBlocking(false);
            this.channel.bind(new InetSocketAddress(port));
            this.channel.register(this.selector, SelectionKey.OP_READ);

            this.bufferSize = this.channel.socket().getReceiveBufferSize();
            this.responses = new ConcurrentLinkedDeque<>();
            this.service = Executors.newFixedThreadPool(threads);
            this.mainService = Executors.newSingleThreadExecutor();
            this.mainService.submit(() -> {
                while (!Thread.currentThread().isInterrupted()) {
                    try {
                        this.selector.select(this::handle, SOCKET_TIMEOUT);
                    } catch (final IOException e) {
                        System.err.println("i/o error in selector:\n\t" + e.getMessage());
                    }
                }
            });
        } catch (final SocketException e) {
            logAndClose("udp error", e);
        } catch (final IOException e) {
            logAndClose("i/o error while starting a server", e);
        } catch (final IllegalArgumentException e) {
            logAndClose("port or threads out of range", e);
        }
    }

    private void logAndClose(final String message, final Exception exception) {
        System.err.printf("%s:%n\t%s%n", message, exception.getMessage());
        close();
    }

    private void handle(final SelectionKey key) {
        try {
            if (key.isWritable()) {
                final ResponseWrapper response = this.responses.poll();
                if (response != null) {
                    this.channel.send(ByteBuffer.wrap(response.response), response.address);
                } else {
                    key.interestOps(SelectionKey.OP_READ);
                }
            } else if (key.isReadable()) {
                final ByteBuffer buffer = ByteBuffer.allocate(bufferSize);
                final SocketAddress address = this.channel.receive(buffer);
                this.service.submit(() -> {
                    this.responses.add(new ResponseWrapper(getServerResponse(getReceivedMessage(buffer)), address));
                    key.interestOps(SelectionKey.OP_WRITE);
                });
            }
        } catch (final IOException e) {
            System.err.println("i/o error while handling a key:\n\t" + e.getMessage());
        } catch (final CancelledKeyException e) {
            System.err.println("key is not valid:\n\t" + e.getMessage());
        }
    }

    @Override
    public void close() {
        close(this.channel);
        close(this.selector);
        close(this.service);
        close(this.mainService);
    }

    private void close(final AutoCloseable closeable) {
        try {
            if (closeable != null) {
                closeable.close();
            }
        } catch (final Exception e) {
            System.err.println("error while closing:\n\t" + e.getMessage());
        }
    }

    private record ResponseWrapper(byte[] response, SocketAddress address) {
    }

    public static void main(final String[] args) {
        serverMain(args, new HelloUDPNonblockingServer());
    }
}
