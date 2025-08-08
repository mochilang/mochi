fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

var letters: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
fun excel_title_to_column(title: String): Int {
    var result: Int = 0
    var i: Int = 0
    while (i < title.length) {
        var ch: String = title.substring(i, i + 1)
        var value: Int = 0
        var idx: Int = 0
        var found: Boolean = false
        while (idx < letters.length) {
            if (letters.substring(idx, idx + 1) == ch) {
                value = idx + 1
                found = true
                break
            }
            idx = idx + 1
        }
        if (!found) {
            panic("title must contain only uppercase A-Z")
        }
        result = (result * 26) + value
        i = i + 1
    }
    return result
}

fun user_main(): Unit {
    println(excel_title_to_column("A"))
    println(excel_title_to_column("B"))
    println(excel_title_to_column("AB"))
    println(excel_title_to_column("Z"))
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
