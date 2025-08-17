fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

fun ln(x: Double): Double {
    var t: Double = (x - 1.0) / (x + 1.0)
    var term: Double = t
    var sum: Double = 0.0
    var k: Int = (1).toInt()
    while (k <= 99) {
        sum = sum + (term / (k.toDouble()))
        term = (term * t) * t
        k = k + 2
    }
    return 2.0 * sum
}

fun log10(x: Double): Double {
    return ln(x) / ln(10.0)
}

fun absf(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun res(x: Int, y: Int): Double {
    if (x == 0) {
        return 0.0
    }
    if (y == 0) {
        return 1.0
    }
    if (x < 0) {
        panic("math domain error")
    }
    return (y.toDouble()) * log10(x.toDouble())
}

fun test_res(): Unit {
    if (absf(res(5, 7) - 4.892790030352132) > 0.0000001) {
        panic("res(5,7) failed")
    }
    if (res(0, 5) != 0.0) {
        panic("res(0,5) failed")
    }
    if (res(3, 0) != 1.0) {
        panic("res(3,0) failed")
    }
}

fun compare(x1: Int, y1: Int, x2: Int, y2: Int): String {
    var r1: Double = res(x1, y1)
    var r2: Double = res(x2, y2)
    if (r1 > r2) {
        return (("Largest number is " + _numToStr(x1)) + " ^ ") + _numToStr(y1)
    }
    if (r2 > r1) {
        return (("Largest number is " + _numToStr(x2)) + " ^ ") + _numToStr(y2)
    }
    return "Both are equal"
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        test_res()
        println(compare(5, 7, 4, 8))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
