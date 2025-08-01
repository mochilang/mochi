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

fun floyd(n: Int): Unit {
    println(("Floyd " + n.toString()) + ":")
    var lowerLeftCorner: BigInteger = (((n * (n - 1)) / 2) + 1).toBigInteger()
    var lastInColumn: BigInteger = lowerLeftCorner
    var lastInRow: Int = 1
    var i: Int = 1
    var row: Int = 1
    var line: String = ""
    while (row <= n) {
        var w: Int = lastInColumn.toString().length
        if (i < lastInRow) {
            line = (line + pad(i.toString(), w)) + " "
            lastInColumn = lastInColumn.add(1.toBigInteger())
        } else {
            line = line + pad(i.toString(), w)
            println(line)
            line = ""
            row = row + 1
            lastInRow = lastInRow + row
            lastInColumn = lowerLeftCorner
        }
        i = i + 1
    }
}

fun pad(s: String, w: Int): String {
    var t: String = s
    while (t.length < w) {
        t = " " + t
    }
    return t
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        floyd(5)
        floyd(14)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
