package info.kgeorgiy.ja.yarunina.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;
import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.net.DatagramPacket;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.NoSuchElementException;

class HelloUDPUtil {
    static final int SOCKET_TIMEOUT = 50;

    private static final String CLIENT_USAGE
            = "usage:\n\t(host_name|host_ip_address) port prefix threads requests_per_thread";

    static void clientMain(final String[] args, final HelloClient client) {
        if (args == null || args.length != 5) {
            System.err.println(CLIENT_USAGE);
            return;
        }
        try {
            client.run(
                    args[0],
                    Integer.parseInt(args[1]),
                    args[2],
                    Integer.parseInt(args[3]),
                    Integer.parseInt(args[4]));
        } catch (final NumberFormatException e) {
            System.err.println("couldn't parse integer argument:\n\t" + e.getMessage());
            System.err.println(CLIENT_USAGE);
        }
    }

    private static final String SERVER_USAGE = "usage:\n\tport threads";

    static void serverMain(final String[] args, final HelloServer server) {
        if (args == null || args.length != 2) {
            System.err.println(SERVER_USAGE);
            return;
        }
        try {
            server.start(Integer.parseInt(args[0]), Integer.parseInt(args[1]));
            Thread.sleep(Long.MAX_VALUE);
            server.close();
        } catch (final NumberFormatException e) {
            System.err.println("couldn't parse integer argument:\n\t" + e.getMessage());
            System.err.println(SERVER_USAGE);
        } catch (final InterruptedException e) {
            System.err.println("server interrupted:\n\t" + e.getMessage());
        }
    }


    static DatagramPacket getPacket(final byte[] buffer, final SocketAddress address) {
        return new DatagramPacket(buffer, buffer.length, address);
    }

    static DatagramPacket getDefaultPacket(final int bufferSize) {
        return new DatagramPacket(new byte[bufferSize], bufferSize);
    }

    static String getReceivedMessage(final DatagramPacket packet) {
        return new String(packet.getData(), packet.getOffset(), packet.getLength(), StandardCharsets.UTF_8);
    }

    static String getReceivedMessage(final ByteBuffer buffer) {
        return String.valueOf(StandardCharsets.UTF_8.decode(buffer.flip()));
    }


    static String getClientRequest(final String prefix, final int thread, final int request) {
        return String.format("%s%d_%d", prefix, thread, request);
    }

    static byte[] getServerResponse(final String request) {
        return String.format("Hello, %s", request).getBytes(StandardCharsets.UTF_8);
    }


    /**
     * Checks if received string contains given prefix and at least two integers in this order,
     * and that the last two integers are equal to thread and request numbers respectively.
     */
    static boolean validate(
            final String prefix,
            final int thread,
            final int request,
            final String received) {
        if (received == null) return false;
        final BackwardScanner scanner = new BackwardScanner(received);
        try {
            return scanner.nextInt() == request && scanner.nextInt() == thread
                    && received.substring(0, scanner.getIndex() + 1).contains(prefix);
        } catch (final NoSuchElementException e) {
            return false;
        }
    }

    private static class BackwardScanner {
        private final String line;
        private int index;

        public BackwardScanner(final String line) {
            this.line = line;
            this.index = line.length() - 1;
        }

        public int nextInt() {
            while (index >= 0 && !Character.isDigit(line.charAt(index))) {
                --index;
            }
            final StringBuilder builder = new StringBuilder();
            while (index >= 0 && Character.isDigit(line.charAt(index))) {
                builder.append(line.charAt(index--));
            }
            if (builder.isEmpty()) {
                throw new NoSuchElementException("there's no next int:(");
            }
            return Integer.parseInt(builder.reverse().toString());
        }

        public int getIndex() {
            return index;
        }
    }
}
