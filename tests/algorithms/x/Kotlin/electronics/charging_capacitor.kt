val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/electronics"

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
        term = (term * y) / (n.toDouble())
        sum = sum + term
        n = n + 1
    }
    if (is_neg as Boolean) {
        return 1.0 / sum
    }
    return sum
}

fun round3(x: Double): Double {
    var scaled: Double = x * 1000.0
    if (scaled >= 0.0) {
        scaled = scaled + 0.5
    } else {
        scaled = scaled - 0.5
    }
    var scaled_int: Int = (scaled.toInt()).toInt()
    return (scaled_int.toDouble()) / 1000.0
}

fun charging_capacitor(source_voltage: Double, resistance: Double, capacitance: Double, time_sec: Double): Double {
    if (source_voltage <= 0.0) {
        panic("Source voltage must be positive.")
    }
    if (resistance <= 0.0) {
        panic("Resistance must be positive.")
    }
    if (capacitance <= 0.0) {
        panic("Capacitance must be positive.")
    }
    var exponent: Double = (0.0 - time_sec) / (resistance * capacitance)
    var voltage: Double = source_voltage * (1.0 - expApprox(exponent))
    return round3(voltage)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(charging_capacitor(0.2, 0.9, 8.4, 0.5))
        println(charging_capacitor(2.2, 3.5, 2.4, 9.0))
        println(charging_capacitor(15.0, 200.0, 20.0, 2.0))
        println(charging_capacitor(20.0, 2000.0, 0.0003, 4.0))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
