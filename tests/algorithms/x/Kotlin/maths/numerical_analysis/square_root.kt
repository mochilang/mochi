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

var r1: Double = square_root_iterative(4.0, 9999, 0.00000000000001)
fun fx(x: Double, a: Double): Double {
    return (((x * x).toDouble()) - a).toDouble()
}

fun fx_derivative(x: Double): Double {
    return 2.0 * x
}

fun get_initial_point(a: Double): Double {
    var start: Double = 2.0
    while (start <= a) {
        start = (start * start).toDouble()
    }
    return start
}

fun abs_float(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun square_root_iterative(a: Double, max_iter: Int, tolerance: Double): Double {
    if (a < 0.0) {
        panic("math domain error")
    }
    var value: Double = get_initial_point(a)
    var i: Int = (0).toInt()
    while (i < max_iter) {
        var prev_value: Double = value
        value = value - (fx(value, a) / fx_derivative(value))
        if (abs_float(prev_value - value) < tolerance) {
            return value
        }
        i = i + 1
    }
    return value
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(r1))
        var r2: Double = square_root_iterative(3.2, 9999, 0.00000000000001)
        println(_numToStr(r2))
        var r3: Double = square_root_iterative(140.0, 9999, 0.00000000000001)
        println(_numToStr(r3))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
