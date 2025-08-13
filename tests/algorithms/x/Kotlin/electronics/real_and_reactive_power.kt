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

fun sqrt(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x
    var i: Int = (0).toInt()
    while (i < 10) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun real_power(apparent_power: Double, power_factor: Double): Double {
    if ((power_factor < (0.0 - 1.0)) || (power_factor > 1.0)) {
        panic("power_factor must be a valid float value between -1 and 1.")
    }
    return apparent_power * power_factor
}

fun reactive_power(apparent_power: Double, power_factor: Double): Double {
    if ((power_factor < (0.0 - 1.0)) || (power_factor > 1.0)) {
        panic("power_factor must be a valid float value between -1 and 1.")
    }
    return apparent_power * sqrt(1.0 - (power_factor * power_factor))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(real_power(100.0, 0.9).toString())
        println(real_power(0.0, 0.8).toString())
        println(real_power(100.0, 0.0 - 0.9).toString())
        println(reactive_power(100.0, 0.9).toString())
        println(reactive_power(0.0, 0.8).toString())
        println(reactive_power(100.0, 0.0 - 0.9).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
