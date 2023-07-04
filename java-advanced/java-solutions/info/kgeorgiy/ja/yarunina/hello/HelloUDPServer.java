package info.kgeorgiy.ja.yarunina.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.stream.IntStream;

import static info.kgeorgiy.ja.yarunina.hello.HelloUDPUtil.*;

public class HelloUDPServer implements HelloServer {
    private ExecutorService service;
    private DatagramSocket socket;

    @Override
    public void start(final int port, final int threads) {
        if (this.socket != null && !this.socket.isClosed()
                || this.service != null && !this.service.isTerminated()) {
            System.err.println("please close the server before restarting");
            return;
        }
        try {
            this.service = Executors.newFixedThreadPool(threads);
            this.socket = new DatagramSocket(port);
            try {
                final int bufferSize = socket.getReceiveBufferSize();
                IntStream.range(0, threads)
                        .<Runnable>mapToObj(thread -> () -> threadAction(bufferSize))
                        .forEach(service::submit);
            } catch (final SocketException e) {
                logAndClose("udp error", e);
            }
        } catch (final IllegalArgumentException e) {
            logAndClose("port or threads out of range", e);
        } catch (final SocketException e) {
            logAndClose("couldn't open or bound the server socket", e);
        } catch (final RejectedExecutionException e) {
            logAndClose("couldn't schedule a task for execution", e);
        }
    }

    private void logAndClose(final String message, final Exception exception) {
        System.err.printf("%s:%n\t%s%n", message, exception.getMessage());
        close();
    }

    private void threadAction(final int bufferSize) {
        final DatagramPacket receivePacket = getDefaultPacket(bufferSize);
        while (!socket.isClosed()) {
            try {
                socket.receive(receivePacket);
                socket.send(getPacket(
                        getServerResponse(getReceivedMessage(receivePacket)),
                        receivePacket.getSocketAddress()));
            } catch (final IOException e) {
                System.err.printf("%s:%n\t%s%n", "i/o error while exchanging packets", e.getMessage());
            }
        }
    }

    @Override
    public void close() {
        if (this.socket != null) {
            this.socket.close();
        }
        if (this.service != null) {
            this.service.close();
        }
    }

    public static void main(final String[] args) {
        serverMain(args, new HelloUDPServer());
    }
}
