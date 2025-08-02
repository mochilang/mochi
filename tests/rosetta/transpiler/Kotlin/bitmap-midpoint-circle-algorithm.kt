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

var g: MutableList<MutableList<String>> = circle(10)
fun initGrid(size: Int): MutableList<MutableList<String>> {
    var g: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    var y: Int = 0
    while (y < size) {
        var row: MutableList<String> = mutableListOf<String>()
        var x: Int = 0
        while (x < size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(" "); _tmp }
            x = x + 1
        }
        g = run { val _tmp = g.toMutableList(); _tmp.add(row); _tmp }
        y = y + 1
    }
    return g
}

fun set(g: MutableList<MutableList<String>>, x: Int, y: Int): Unit {
    if ((((((x >= 0) && (x < (g[0]!!).size) as Boolean)) && (y >= 0) as Boolean)) && (y < g.size)) {
        (g[y]!!)[x] = "#"
    }
}

fun circle(r: Int): MutableList<MutableList<String>> {
    var size: BigInteger = ((r * 2) + 1).toBigInteger()
    var g: MutableList<MutableList<String>> = initGrid((size.toInt()))
    var x: Int = r
    var y: Int = 0
    var err: BigInteger = (1 - r).toBigInteger()
    while (y <= x) {
        set(g, r + x, r + y)
        set(g, r + y, r + x)
        set(g, r - x, r + y)
        set(g, r - y, r + x)
        set(g, r - x, r - y)
        set(g, r - y, r - x)
        set(g, r + x, r - y)
        set(g, r + y, r - x)
        y = y + 1
        if (err.compareTo((0).toBigInteger()) < 0) {
            err = (err.add((2 * y).toBigInteger())).add((1).toBigInteger())
        } else {
            x = x - 1
            err = (err.add((2 * (y - x)).toBigInteger())).add((1).toBigInteger())
        }
    }
    return g
}

fun trimRight(row: MutableList<String>): String {
    var end: Int = row.size
    while ((end > 0) && (row[end - 1]!! == " ")) {
        end = end - 1
    }
    var s: String = ""
    var i: Int = 0
    while (i < end) {
        s = s + row[i]!!
        i = i + 1
    }
    return s
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (row in g) {
            println(trimRight(row))
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
