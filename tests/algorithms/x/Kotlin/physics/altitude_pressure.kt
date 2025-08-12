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

fun to_float(x: Int): Double {
    return x * 1.0
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
        var denom: Double = to_float((2 * k) + 1)
        sum = sum + (term / denom)
        term = term * y2
        k = k + 1
    }
    return 2.0 * sum
}

fun exp(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = (1).toInt()
    while (n < 20) {
        term = (term * x) / to_float(n)
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun pow_float(base: Double, exponent: Double): Double {
    return exp(exponent * ln(base))
}

fun get_altitude_at_pressure(pressure: Double): Double {
    if (pressure > 101325.0) {
        panic("Value Higher than Pressure at Sea Level !")
    }
    if (pressure < 0.0) {
        panic("Atmospheric Pressure can not be negative !")
    }
    var ratio: Double = pressure / 101325.0
    return 44330.0 * (1.0 - pow_float(ratio, 1.0 / 5.5255))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(get_altitude_at_pressure(100000.0).toString())
        println(get_altitude_at_pressure(101325.0).toString())
        println(get_altitude_at_pressure(80000.0).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
