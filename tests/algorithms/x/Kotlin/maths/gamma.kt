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

var PI: Double = 3.141592653589793
fun absf(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun sqrt(x: Double): Double {
    if (x < 0.0) {
        panic("sqrt domain error")
    }
    var guess: Double = x / 2.0
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun ln(x: Double): Double {
    if (x <= 0.0) {
        panic("ln domain error")
    }
    var y: Double = (x - 1.0) / (x + 1.0)
    var y2: Double = y * y
    var term: Double = y
    var sum: Double = 0.0
    var k: Int = (0).toInt()
    while (k < 10) {
        var denom: Double = (((2 * k) + 1).toDouble())
        sum = sum + (term / denom)
        term = term * y2
        k = k + 1
    }
    return 2.0 * sum
}

fun exp_series(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = (1).toInt()
    while (n < 20) {
        term = (term * x) / ((n.toDouble()))
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun powf(base: Double, exponent: Double): Double {
    if (base <= 0.0) {
        return 0.0
    }
    return exp_series(exponent * ln(base))
}

fun integrand(x: Double, z: Double): Double {
    return powf(x, z - 1.0) * exp_series(0.0 - x)
}

fun gamma_iterative(num: Double): Double {
    if (num <= 0.0) {
        panic("math domain error")
    }
    var step: Double = 0.001
    var limit: Double = 100.0
    var x: Double = step
    var total: Double = 0.0
    while (x < limit) {
        total = total + (integrand(x, num) * step)
        x = x + step
    }
    return total
}

fun gamma_recursive(num: Double): Double {
    if (num <= 0.0) {
        panic("math domain error")
    }
    if (num > 171.5) {
        panic("math range error")
    }
    var int_part: Int = ((num.toInt())).toInt()
    var frac: Double = num - ((int_part.toDouble()))
    if (!(((absf(frac) < 0.000001) || (absf(frac - 0.5) < 0.000001)) as Boolean)) {
        panic("num must be an integer or a half-integer")
    }
    if (absf(num - 0.5) < 0.000001) {
        return sqrt(PI)
    }
    if (absf(num - 1.0) < 0.000001) {
        return 1.0
    }
    return (num - 1.0) * gamma_recursive(num - 1.0)
}

fun user_main(): Unit {
    println(gamma_iterative(5.0))
    println(gamma_recursive(5.0))
    println(gamma_recursive(0.5))
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
