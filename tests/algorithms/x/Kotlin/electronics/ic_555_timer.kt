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

fun astable_frequency(resistance_1: Double, resistance_2: Double, capacitance: Double): Double {
    if ((((resistance_1 <= 0.0) || (resistance_2 <= 0.0) as Boolean)) || (capacitance <= 0.0)) {
        panic("All values must be positive")
    }
    return (1.44 / ((resistance_1 + (2.0 * resistance_2)) * capacitance)) * 1000000.0
}

fun astable_duty_cycle(resistance_1: Double, resistance_2: Double): Double {
    if ((resistance_1 <= 0.0) || (resistance_2 <= 0.0)) {
        panic("All values must be positive")
    }
    return ((resistance_1 + resistance_2) / (resistance_1 + (2.0 * resistance_2))) * 100.0
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(astable_frequency(45.0, 45.0, 7.0))
        println(astable_duty_cycle(45.0, 45.0))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
