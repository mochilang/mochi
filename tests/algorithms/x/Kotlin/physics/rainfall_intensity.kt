fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

var r1: Double = rainfall_intensity(1000.0, 0.2, 11.6, 0.81, 10.0, 60.0)
fun exp_approx(x: Double): Double {
    var y: Double = x
    var is_neg: Boolean = false
    if (x < 0.0) {
        is_neg = true
        y = 0.0 - x
    }
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = (1).toInt()
    while (n < 30) {
        term = (term * y) / ((n.toDouble()))
        sum = sum + term
        n = n + 1
    }
    if ((is_neg as Boolean)) {
        return 1.0 / sum
    }
    return sum
}

fun ln_series(x: Double): Double {
    var t: Double = (x - 1.0) / (x + 1.0)
    var term: Double = t
    var sum: Double = 0.0
    var n: Int = (1).toInt()
    while (n <= 19) {
        sum = sum + (term / ((n.toDouble())))
        term = (term * t) * t
        n = n + 2
    }
    return 2.0 * sum
}

fun ln(x: Double): Double {
    var y: Double = x
    var k: Int = (0).toInt()
    while (y >= 10.0) {
        y = y / 10.0
        k = k + 1
    }
    while (y < 1.0) {
        y = y * 10.0
        k = k - 1
    }
    return ln_series(y) + (((k.toDouble())) * ln_series(10.0))
}

fun powf(base: Double, exponent: Double): Double {
    return exp_approx(exponent * ln(base))
}

fun rainfall_intensity(coefficient_k: Double, coefficient_a: Double, coefficient_b: Double, coefficient_c: Double, return_period: Double, duration: Double): Double {
    if (coefficient_k <= 0.0) {
        panic("All parameters must be positive.")
    }
    if (coefficient_a <= 0.0) {
        panic("All parameters must be positive.")
    }
    if (coefficient_b <= 0.0) {
        panic("All parameters must be positive.")
    }
    if (coefficient_c <= 0.0) {
        panic("All parameters must be positive.")
    }
    if (return_period <= 0.0) {
        panic("All parameters must be positive.")
    }
    if (duration <= 0.0) {
        panic("All parameters must be positive.")
    }
    var numerator: Double = coefficient_k * powf(return_period, coefficient_a)
    var denominator: Double = powf(duration + coefficient_b, coefficient_c)
    return numerator / denominator
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(r1.toString())
        var r2: Double = rainfall_intensity(1000.0, 0.2, 11.6, 0.81, 10.0, 30.0)
        println(r2.toString())
        var r3: Double = rainfall_intensity(1000.0, 0.2, 11.6, 0.81, 5.0, 60.0)
        println(r3.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
