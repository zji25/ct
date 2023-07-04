package info.kgeorgiy.ja.yarunina.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.IntStream;

import static info.kgeorgiy.ja.yarunina.hello.HelloUDPUtil.*;

public class HelloUDPClient implements HelloClient {
    @Override
    public void run(
            final String host,
            final int port,
            final String prefix,
            final int threads,
            final int requests) {
        try {
            final InetSocketAddress serverAddress = new InetSocketAddress(host, port);
            if (serverAddress.isUnresolved()) {
                System.err.println("couldn't resolve the hostname into an inet address.");
                return;
            }
            try (final ExecutorService service = Executors.newFixedThreadPool(threads)) {
                IntStream.range(1, threads + 1)
                        .<Runnable>mapToObj(thread -> () -> threadAction(prefix, thread, requests, serverAddress))
                        .forEach(service::submit);
            }
        } catch (final IllegalArgumentException e) {
            System.err.println("port or threads out of range:\n\t" +  e.getMessage());
        }
    }

    private void threadAction(
            final String prefix,
            final int thread,
            final int requests,
            final InetSocketAddress serverAddress) {
        try (final DatagramSocket socket = new DatagramSocket()) {
            try {
                socket.setSoTimeout(SOCKET_TIMEOUT);
                final int bufferSize = socket.getReceiveBufferSize();
                final DatagramPacket packetToReceive = getDefaultPacket(bufferSize);

                IntStream.range(1, requests + 1)
                        .forEach(request ->
                                processRequest(prefix, thread, request, serverAddress, socket, packetToReceive));
            } catch (final SocketException e) {
                System.err.println("UDP error:\n\t" + e.getMessage());
            }
        } catch (final SocketException e) {
            System.err.println("couldn't open or bound the client socket:\n\t" + e.getMessage());
        }
    }

    private static void processRequest(
            final String prefix,
            final int thread,
            final int request,
            final InetSocketAddress serverAddress,
            final DatagramSocket socket,
            final DatagramPacket packetToReceive) {
        final String sent = getClientRequest(prefix, thread, request);
        final DatagramPacket packetToSend = getPacket(sent.getBytes(StandardCharsets.UTF_8), serverAddress);

        String received = null;
        while (!validate(prefix, thread, request, received)) {
            try {
                socket.send(packetToSend);
                socket.receive(packetToReceive);
                received = getReceivedMessage(packetToReceive);
            } catch (final IOException e) {
                System.err.println("i/o error while exchanging packets:\n\t" + e.getMessage());
            }
        }
        System.out.println("request: " + sent);
        System.out.println("response: " + received);
    }

    public static void main(final String[] args) {
        clientMain(args, new HelloUDPClient());
    }
}

