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

fun repeat(ch: String, n: Int): String {
    var s: String = ""
    var i: Int = 0
    while (i < n) {
        s = s + ch
        i = i + 1
    }
    return s
}

fun cubLine(n: Int, dx: Int, dy: Int, cde: String): Unit {
    var line: String = repeat(" ", n + 1) + cde.substring(0, 1)
    var d: BigInteger = (((9 * dx) - 1).toBigInteger())
    while (d.compareTo((0).toBigInteger()) > 0) {
        line = line + cde.substring(1, 2)
        d = d.subtract((1).toBigInteger())
    }
    line = line + cde.substring(0, 1)
    line = (line + repeat(" ", dy)) + cde.substring(2, cde.length)
    println(line)
}

fun cuboid(dx: Int, dy: Int, dz: Int): Unit {
    println(((((("cuboid " + dx.toString()) + " ") + dy.toString()) + " ") + dz.toString()) + ":")
    cubLine(dy + 1, dx, 0, "+-")
    var i: Int = 1
    while (i <= dy) {
        cubLine((dy - i) + 1, dx, i - 1, "/ |")
        i = i + 1
    }
    cubLine(0, dx, dy, "+-|")
    var j: BigInteger = ((((4 * dz) - dy) - 2).toBigInteger())
    while (j.compareTo((0).toBigInteger()) > 0) {
        cubLine(0, dx, dy, "| |")
        j = j.subtract((1).toBigInteger())
    }
    cubLine(0, dx, dy, "| +")
    i = 1
    while (i <= dy) {
        cubLine(0, dx, dy - i, "| /")
        i = i + 1
    }
    cubLine(0, dx, 0, "+-\n")
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        cuboid(2, 3, 4)
        println("")
        cuboid(1, 1, 1)
        println("")
        cuboid(6, 2, 1)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
