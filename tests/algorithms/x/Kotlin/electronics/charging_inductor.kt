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

fun expApprox(x: Double): Double {
    if (x < 0.0) {
        return 1.0 / expApprox(0.0 - x)
    }
    if (x > 1.0) {
        var half: Double = expApprox(x / 2.0)
        return half * half
    }
    var sum: Double = 1.0
    var term: Double = 1.0
    var n: Int = (1).toInt()
    while (n < 20) {
        term = (term * x) / ((n.toDouble()))
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun floor(x: Double): Double {
    var i: Int = ((x.toInt())).toInt()
    if (((i.toDouble())) > x) {
        i = i - 1
    }
    return (i.toDouble())
}

fun pow10(n: Int): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < n) {
        result = result * 10.0
        i = i + 1
    }
    return result
}

fun round(x: Double, n: Int): Double {
    var m: Double = pow10(n)
    return floor((x * m) + 0.5) / m
}

fun charging_inductor(source_voltage: Double, resistance: Double, inductance: Double, time: Double): Double {
    if (source_voltage <= 0.0) {
        panic("Source voltage must be positive.")
    }
    if (resistance <= 0.0) {
        panic("Resistance must be positive.")
    }
    if (inductance <= 0.0) {
        panic("Inductance must be positive.")
    }
    var exponent: Double = ((0.0 - time) * resistance) / inductance
    var current: Double = (source_voltage / resistance) * (1.0 - expApprox(exponent))
    return round(current, 3)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(charging_inductor(5.8, 1.5, 2.3, 2.0))
        println(charging_inductor(8.0, 5.0, 3.0, 2.0))
        println(charging_inductor(8.0, 5.0 * pow10(2), 3.0, 2.0))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
