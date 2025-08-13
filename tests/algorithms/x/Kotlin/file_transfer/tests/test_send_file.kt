var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Long {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed)
    } else {
        kotlin.math.abs(System.nanoTime())
    }
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

data class ConnMock(var recv_called: Int = 0, var send_called: Int = 0, var close_called: Int = 0)
data class SocketMock(var bind_called: Int = 0, var listen_called: Int = 0, var accept_called: Int = 0, var shutdown_called: Int = 0, var close_called: Int = 0, var conn: ConnMock = ConnMock(recv_called = 0, send_called = 0, close_called = 0))
data class FileMock(var read_called: Int = 0, var data: MutableList<Int> = mutableListOf<Int>())
fun make_conn_mock(): ConnMock {
    return ConnMock(recv_called = 0, send_called = 0, close_called = 0)
}

fun conn_recv(conn: ConnMock, size: Int): Int {
    conn.recv_called = conn.recv_called + 1
    return 0
}

fun conn_send(conn: ConnMock, data: Int): Unit {
    conn.send_called = conn.send_called + 1
}

fun conn_close(conn: ConnMock): Unit {
    conn.close_called = conn.close_called + 1
}

fun make_socket_mock(conn: ConnMock): SocketMock {
    return SocketMock(bind_called = 0, listen_called = 0, accept_called = 0, shutdown_called = 0, close_called = 0, conn = conn)
}

fun socket_bind(sock: SocketMock): Unit {
    sock.bind_called = sock.bind_called + 1
}

fun socket_listen(sock: SocketMock): Unit {
    sock.listen_called = sock.listen_called + 1
}

fun socket_accept(sock: SocketMock): ConnMock {
    sock.accept_called = sock.accept_called + 1
    return sock.conn
}

fun socket_shutdown(sock: SocketMock): Unit {
    sock.shutdown_called = sock.shutdown_called + 1
}

fun socket_close(sock: SocketMock): Unit {
    sock.close_called = sock.close_called + 1
}

fun make_file_mock(values: MutableList<Int>): FileMock {
    return FileMock(read_called = 0, data = values)
}

fun file_read(f: FileMock, size: Int): Int {
    if (f.read_called < (f.data).size) {
        var value: Int = ((f.data)[f.read_called]!!).toInt()
        f.read_called = f.read_called + 1
        return value
    }
    f.read_called = f.read_called + 1
    return 0
}

fun file_open(): FileMock {
    return make_file_mock(mutableListOf(1, 0))
}

fun send_file(sock: SocketMock, f: FileMock): Unit {
    socket_bind(sock)
    socket_listen(sock)
    var conn: ConnMock = socket_accept(sock)
    var _u1: Int = (conn_recv(conn, 1024)).toInt()
    var data: Int = (file_read(f, 1024)).toInt()
    while (data != 0) {
        conn_send(conn, data)
        data = file_read(f, 1024)
    }
    conn_close(conn)
    socket_shutdown(sock)
    socket_close(sock)
}

fun test_send_file_running_as_expected(): String {
    var conn: ConnMock = make_conn_mock()
    var sock: SocketMock = make_socket_mock(conn)
    var f: FileMock = file_open()
    send_file(sock, f)
    if ((((((((((((((((sock.bind_called == 1) && (sock.listen_called == 1) as Boolean)) && (sock.accept_called == 1) as Boolean)) && (conn.recv_called == 1) as Boolean)) && (f.read_called >= 1) as Boolean)) && (conn.send_called == 1) as Boolean)) && (conn.close_called == 1) as Boolean)) && (sock.shutdown_called == 1) as Boolean)) && (sock.close_called == 1)) {
        return "pass"
    }
    return "fail"
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(test_send_file_running_as_expected())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
