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

fun createFile(fs: MutableMap<String, Boolean>, fn: String): Unit {
    if (fn in fs) {
        println(("open " + fn) + ": file exists")
    } else {
        (fs)[fn] = false
        println(("file " + fn) + " created!")
    }
}

fun createDir(fs: MutableMap<String, Boolean>, dn: String): Unit {
    if (dn in fs) {
        println(("mkdir " + dn) + ": file exists")
    } else {
        (fs)[dn] = true
        println(("directory " + dn) + " created!")
    }
}

fun user_main(): Unit {
    var fs: MutableMap<String, Boolean> = mutableMapOf<String, Boolean>()
    (fs)["docs"] = true
    createFile(fs, "input.txt")
    createFile(fs, "/input.txt")
    createDir(fs, "docs")
    createDir(fs, "/docs")
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
