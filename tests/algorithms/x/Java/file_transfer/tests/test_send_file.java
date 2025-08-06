public class Main {
    static class ConnMock {
        int recv_called;
        int send_called;
        int close_called;
        ConnMock(int recv_called, int send_called, int close_called) {
            this.recv_called = recv_called;
            this.send_called = send_called;
            this.close_called = close_called;
        }
        ConnMock() {}
        @Override public String toString() {
            return String.format("{'recv_called': %s, 'send_called': %s, 'close_called': %s}", String.valueOf(recv_called), String.valueOf(send_called), String.valueOf(close_called));
        }
    }

    static class SocketMock {
        int bind_called;
        int listen_called;
        int accept_called;
        int shutdown_called;
        int close_called;
        ConnMock conn;
        SocketMock(int bind_called, int listen_called, int accept_called, int shutdown_called, int close_called, ConnMock conn) {
            this.bind_called = bind_called;
            this.listen_called = listen_called;
            this.accept_called = accept_called;
            this.shutdown_called = shutdown_called;
            this.close_called = close_called;
            this.conn = conn;
        }
        SocketMock() {}
        @Override public String toString() {
            return String.format("{'bind_called': %s, 'listen_called': %s, 'accept_called': %s, 'shutdown_called': %s, 'close_called': %s, 'conn': %s}", String.valueOf(bind_called), String.valueOf(listen_called), String.valueOf(accept_called), String.valueOf(shutdown_called), String.valueOf(close_called), String.valueOf(conn));
        }
    }

    static class FileMock {
        int read_called;
        int[] data;
        FileMock(int read_called, int[] data) {
            this.read_called = read_called;
            this.data = data;
        }
        FileMock() {}
        @Override public String toString() {
            return String.format("{'read_called': %s, 'data': %s}", String.valueOf(read_called), String.valueOf(data));
        }
    }


    static ConnMock make_conn_mock() {
        return new ConnMock(0, 0, 0);
    }

    static int conn_recv(ConnMock conn, int size) {
conn.recv_called = conn.recv_called + 1;
        return 0;
    }

    static void conn_send(ConnMock conn, int data) {
conn.send_called = conn.send_called + 1;
    }

    static void conn_close(ConnMock conn) {
conn.close_called = conn.close_called + 1;
    }

    static SocketMock make_socket_mock(ConnMock conn) {
        return new SocketMock(0, 0, 0, 0, 0, conn);
    }

    static void socket_bind(SocketMock sock) {
sock.bind_called = sock.bind_called + 1;
    }

    static void socket_listen(SocketMock sock) {
sock.listen_called = sock.listen_called + 1;
    }

    static ConnMock socket_accept(SocketMock sock) {
sock.accept_called = sock.accept_called + 1;
        return sock.conn;
    }

    static void socket_shutdown(SocketMock sock) {
sock.shutdown_called = sock.shutdown_called + 1;
    }

    static void socket_close(SocketMock sock) {
sock.close_called = sock.close_called + 1;
    }

    static FileMock make_file_mock(int[] values) {
        return new FileMock(0, values);
    }

    static int file_read(FileMock f, int size) {
        if (f.read_called < f.data.length) {
            int value = f.data[f.read_called];
f.read_called = f.read_called + 1;
            return value;
        }
f.read_called = f.read_called + 1;
        return 0;
    }

    static FileMock file_open() {
        return make_file_mock(((int[])(new int[]{1, 0})));
    }

    static void send_file(SocketMock sock, FileMock f) {
        socket_bind(sock);
        socket_listen(sock);
        ConnMock conn = socket_accept(sock);
        int _v = conn_recv(conn, 1024);
        int data = file_read(f, 1024);
        while (data != 0) {
            conn_send(conn, data);
            data = file_read(f, 1024);
        }
        conn_close(conn);
        socket_shutdown(sock);
        socket_close(sock);
    }

    static String test_send_file_running_as_expected() {
        ConnMock conn_1 = make_conn_mock();
        SocketMock sock = make_socket_mock(conn_1);
        FileMock f = file_open();
        send_file(sock, f);
        if (sock.bind_called == 1 && sock.listen_called == 1 && sock.accept_called == 1 && conn_1.recv_called == 1 && f.read_called >= 1 && conn_1.send_called == 1 && conn_1.close_called == 1 && sock.shutdown_called == 1 && sock.close_called == 1) {
            return "pass";
        }
        return "fail";
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(test_send_file_running_as_expected());
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
