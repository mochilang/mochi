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

var b: Int = ord("a")
var r: Int = ord("π")
var s: String = "aπ"
fun ord(ch: String): Int {
    if (ch == "a") {
        return 97
    }
    if (ch == "π") {
        return 960
    }
    if (ch == "A") {
        return 65
    }
    return 0
}

fun chr(n: Int): String {
    if (n == 97) {
        return "a"
    }
    if (n == 960) {
        return "π"
    }
    if (n == 65) {
        return "A"
    }
    return "?"
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println((((b.toString() + " ") + r.toString()) + " ") + s)
        println(((("string cast to []rune: [" + b.toString()) + " ") + r.toString()) + "]")
        println((("    string range loop: " + b.toString()) + " ") + r.toString())
        println("         string bytes: 0x61 0xcf 0x80")
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
