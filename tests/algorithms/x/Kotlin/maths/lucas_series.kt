fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

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

fun recursive_lucas_number(n: Int): Int {
    if (n == 0) {
        return 2
    }
    if (n == 1) {
        return 1
    }
    return recursive_lucas_number(n - 1) + recursive_lucas_number(n - 2)
}

fun dynamic_lucas_number(n: Int): Int {
    var a: Int = (2).toInt()
    var b: Int = (1).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        var next: Int = (a + b).toInt()
        a = b
        b = next
        i = i + 1
    }
    return a
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(recursive_lucas_number(1)))
        println(_numToStr(recursive_lucas_number(20)))
        println(_numToStr(recursive_lucas_number(0)))
        println(_numToStr(recursive_lucas_number(5)))
        println(_numToStr(dynamic_lucas_number(1)))
        println(_numToStr(dynamic_lucas_number(20)))
        println(_numToStr(dynamic_lucas_number(0)))
        println(_numToStr(dynamic_lucas_number(25)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
