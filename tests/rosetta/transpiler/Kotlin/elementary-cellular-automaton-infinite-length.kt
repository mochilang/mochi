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

fun pow2(n: Int): Int {
    var p: Int = 1
    var i: Int = 0
    while (i < n) {
        p = p * 2
        i = i + 1
    }
    return p
}

fun btoi(b: Boolean): Int {
    if (b as Boolean) {
        return 1
    }
    return 0
}

fun addNoCells(cells: String): String {
    var cells: String = cells
    var l: String = "O"
    var r: String = "O"
    if (cells.substring(0, 1) == "O") {
        l = "."
    }
    if (cells.substring(cells.length - 1, cells.length) == "O") {
        r = "."
    }
    cells = (l + cells) + r
    cells = (l + cells) + r
    return cells
}

fun step(cells: String, ruleVal: Int): String {
    var newCells: String = ""
    var i: Int = 0
    while (i < (cells.length - 2)) {
        var bin: Int = 0
        var b: Int = 2
        var n: Int = i
        while (n < (i + 3)) {
            bin = bin + (btoi(cells.substring(n, n + 1) == "O") * pow2(b))
            b = b - 1
            n = n + 1
        }
        var a: String = "."
        if ((Math.floorMod((ruleVal / pow2(bin)), 2)) == 1) {
            a = "O"
        }
        newCells = newCells + a
        i = i + 1
    }
    return newCells
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

fun evolve(l: Int, ruleVal: Int): Unit {
    println((" Rule #" + ruleVal.toString()) + ":")
    var cells: String = "O"
    var x: Int = 0
    while (x < l) {
        cells = addNoCells(cells)
        var width: BigInteger = (40 + (cells.length / 2)).toBigInteger()
        var spaces: String = repeat(" ", (width.subtract(cells.length.toBigInteger())).toInt())
        println(spaces + cells)
        cells = step(cells, ruleVal)
        x = x + 1
    }
}

fun user_main(): Unit {
    for (r in mutableListOf(90, 30)) {
        evolve(25, r)
        println("")
    }
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
