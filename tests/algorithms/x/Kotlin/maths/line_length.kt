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

fun sqrt_newton(n: Double): Double {
    if (n == 0.0) {
        return 0.0
    }
    var x: Double = n
    var i: Int = (0).toInt()
    while (i < 20) {
        x = (x + (n / x)) / 2.0
        i = i + 1
    }
    return x
}

fun hypot(a: Double, b: Double): Double {
    return sqrt_newton((a * a) + (b * b))
}

fun line_length(fnc: (Double) -> Double, x_start: Double, x_end: Double, steps: Int): Double {
    var x1: Double = x_start
    var fx1: Double = (fnc(x_start)).toDouble()
    var length: Double = 0.0
    var i: Int = (0).toInt()
    var step: Double = (x_end - x_start) / (1.0 * (steps).toDouble())
    while (i < steps) {
        var x2: Double = step + x1
        var fx2: Double = (fnc(x2)).toDouble()
        length = length + hypot(x2 - x1, fx2 - fx1)
        x1 = x2
        fx1 = fx2
        i = i + 1
    }
    return length
}

fun f1(x: Double): Double {
    return x
}

fun f2(x: Double): Double {
    return 1.0
}

fun f3(x: Double): Double {
    return ((x * x).toDouble()) / 10.0
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(line_length(::f1, 0.0, 1.0, 10))
        println(line_length(::f2, 0.0 - 5.5, 4.5, 100))
        println(line_length(::f3, 0.0, 10.0, 1000))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
