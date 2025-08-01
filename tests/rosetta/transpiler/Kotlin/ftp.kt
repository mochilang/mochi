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

data class FileInfo(var name: String, var size: Int, var kind: String)
data class FTPConn(var dir: String)
var serverData: MutableMap<String, MutableMap<String, String>> = mutableMapOf<String, MutableMap<String, String>>("pub" to (mutableMapOf<String, String>("somefile.bin" to ("This is a file from the FTP server."), "readme.txt" to ("Hello from ftp."))))
var serverNames: MutableMap<String, MutableList<String>> = mutableMapOf<String, MutableList<String>>("pub" to (mutableListOf("somefile.bin", "readme.txt")))
fun connect(hostport: String): FTPConn {
    println("Connected to " + hostport)
    return FTPConn(dir = "/")
}

fun login(conn: FTPConn, user: String, pass: String): Unit {
    println("Logged in as " + user)
}

fun changeDir(conn: FTPConn, dir: String): Unit {
    conn.dir = dir
}

fun list(conn: FTPConn): MutableList<FileInfo> {
    var names: MutableList<String> = (serverNames)[conn.dir] as MutableList<String>
    var dataDir: MutableMap<String, String> = (serverData)[conn.dir] as MutableMap<String, String>
    var out: MutableList<FileInfo> = mutableListOf<FileInfo>()
    for (name in names) {
        var content: String = (dataDir)[name] as String
        out = run { val _tmp = out.toMutableList(); _tmp.add(FileInfo(name = name, size = content.length, kind = "file")); _tmp } as MutableList<FileInfo>
    }
    return out
}

fun retrieve(conn: FTPConn, name: String): String {
    return (((serverData)[conn.dir] as MutableMap<String, String>) as MutableMap<String, String>)[name] as String
}

fun user_main(): Unit {
    var conn: FTPConn = connect("localhost:21")
    login(conn, "anonymous", "anonymous")
    changeDir(conn, "pub")
    println(conn.dir)
    var files: MutableList<FileInfo> = list(conn)
    for (f in files) {
        println((f.name + " ") + f.size.toString())
    }
    var data: String = retrieve(conn, "somefile.bin")
    println(("Wrote " + data.length.toString()) + " bytes to somefile.bin")
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
