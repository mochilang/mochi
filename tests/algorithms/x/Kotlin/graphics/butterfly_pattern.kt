import java.math.BigInteger

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

fun repeat_char(ch: String, count: Int): String {
    var result: String = ""
    var i: Int = (0).toInt()
    while (i < count) {
        result = result + ch
        i = i + 1
    }
    return result
}

fun butterfly_pattern(n: Int): String {
    var lines: MutableList<String> = mutableListOf<String>()
    var i: Int = (1).toInt()
    while (i < n) {
        var left: String = repeat_char("*", i)
        var mid: String = repeat_char(" ", (2 * (n - i)) - 1)
        var right: String = repeat_char("*", i)
        lines = run { val _tmp = lines.toMutableList(); _tmp.add((left + mid) + right); _tmp }
        i = i + 1
    }
    lines = run { val _tmp = lines.toMutableList(); _tmp.add(repeat_char("*", (2 * n) - 1)); _tmp }
    var j: Int = (n - 1).toInt()
    while (j > 0) {
        var left: String = repeat_char("*", j)
        var mid: String = repeat_char(" ", (2 * (n - j)) - 1)
        var right: String = repeat_char("*", j)
        lines = run { val _tmp = lines.toMutableList(); _tmp.add((left + mid) + right); _tmp }
        j = j - 1
    }
    var out: String = ""
    var k: Int = (0).toInt()
    while (k < lines.size) {
        if (k > 0) {
            out = out + "\n"
        }
        out = out + lines[k]!!
        k = k + 1
    }
    return out
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(butterfly_pattern(3))
        println(butterfly_pattern(5))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
