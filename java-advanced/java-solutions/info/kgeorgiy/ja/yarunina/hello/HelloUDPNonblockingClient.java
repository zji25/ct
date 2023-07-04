package info.kgeorgiy.ja.yarunina.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import static info.kgeorgiy.ja.yarunina.hello.HelloUDPUtil.*;

public class HelloUDPNonblockingClient implements HelloClient {
    @Override
    public void run(
            final String host,
            final int port,
            final String prefix,
            final int threads,
            final int requests) {
        final InetSocketAddress serverAddress = new InetSocketAddress(host, port);
        if (serverAddress.isUnresolved()) {
            System.err.println("couldn't resolve the hostname into an inet address.");
            return;
        }
        try (final Selector selector = Selector.open()) {
            try (final Channels channels = new Channels(threads, selector)) {
                while (!Thread.currentThread().isInterrupted() && !selector.keys().isEmpty()) {
                    if (selector.select(key -> handle(key, serverAddress, prefix, requests), SOCKET_TIMEOUT) == 0) {
                        selector.keys().forEach(key -> key.interestOps(SelectionKey.OP_WRITE));
                    }
                }
            } catch (final IOException e) {
                System.err.println("i/o error while creating channels:\n\t" + e.getMessage());
            }
        } catch (final IOException e) {
            System.err.println("selector i/o error:\n\t" + e.getMessage());
        }
    }

    private static void handle(
            final SelectionKey key,
            final InetSocketAddress serverAddress,
            final String prefix,
            final int requests) {
        final DatagramChannel channel = (DatagramChannel) key.channel();
        final Attachment attachment = (Attachment) key.attachment();
        try {
            if (key.isWritable()) {
                final String sent = getClientRequest(prefix, attachment.thread, attachment.request);
                channel.send(ByteBuffer.wrap(sent.getBytes(StandardCharsets.UTF_8)), serverAddress);
                System.out.println("request: " + sent);
                key.interestOps(SelectionKey.OP_READ);
            } else if (key.isReadable()) {
                channel.receive(attachment.buffer.clear());
                final String received = getReceivedMessage(attachment.buffer);
                if (validate(prefix, attachment.thread, attachment.request, received)) {
                    System.out.println("response: " + received);
                    key.interestOps(SelectionKey.OP_WRITE);
                    if (++attachment.request == requests + 1) {
                        channel.close();
                    }
                }
            }
        } catch (final IOException e) {
            System.err.println("i/o error while handling a key:\n\t" + e.getMessage());
        }
    }

    private static class Channels implements AutoCloseable {
        private final List<DatagramChannel> channels;

        public Channels(final int amount, final Selector selector) throws IOException {
            this.channels = new ArrayList<>();
            for (int i = 0; i < amount; ++i) {
                final DatagramChannel channel = DatagramChannel.open();
                channel.configureBlocking(false);
                channel.register(
                        selector,
                        SelectionKey.OP_WRITE,
                        new Attachment(i + 1, channel.socket().getReceiveBufferSize()));
                this.channels.add(channel);
            }
        }

        @Override
        public void close() {
            this.channels.forEach(channel -> {
                try {
                    channel.close();
                } catch (final IOException e) {
                    System.err.println("i/o error while closing a channel:\n\t" + e.getMessage());
                }
            });
        }
    }

    private static class Attachment {
        final int thread;
        int request = 1;
        final ByteBuffer buffer;

        public Attachment(final int thread, final int bufferSize) {
            this.thread = thread;
            this.buffer = ByteBuffer.allocate(bufferSize);
        }
    }

    public static void main(final String[] args) {
        clientMain(args, new HelloUDPNonblockingClient());
    }
}
