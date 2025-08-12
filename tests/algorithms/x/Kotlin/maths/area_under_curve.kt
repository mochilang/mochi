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

fun abs_float(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    } else {
        return x
    }
}

fun trapezoidal_area(f: (Double) -> Double, x_start: Double, x_end: Double, steps: Int): Double {
    var step: Double = (x_end - x_start) / ((steps.toDouble()))
    var x1: Double = x_start
    var fx1: Double = f(x_start)
    var area: Double = 0.0
    var i: Int = (0).toInt()
    while (i < steps) {
        var x2: Double = x1 + step
        var fx2: Double = f(x2)
        area = area + ((abs_float(fx2 + fx1) * step) / 2.0)
        x1 = x2
        fx1 = fx2
        i = (i + 1).toInt()
    }
    return area
}

fun f(x: Double): Double {
    return ((x * x) * x) + (x * x)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("f(x) = x^3 + x^2")
        println("The area between the curve, x = -5, x = 5 and the x axis is:")
        var i: Int = (10).toInt()
        while (i <= 100000) {
            var result: Double = trapezoidal_area(::f, 0.0 - 5.0, 5.0, i)
            println((("with " + i.toString()) + " steps: ") + result.toString())
            i = (i * 10).toInt()
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
