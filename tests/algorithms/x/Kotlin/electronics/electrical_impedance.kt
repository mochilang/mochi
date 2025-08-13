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

fun sqrtApprox(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x / 2.0
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun electrical_impedance(resistance: Double, reactance: Double, impedance: Double): MutableMap<String, Double> {
    var zero_count: Int = (0).toInt()
    if (resistance == 0.0) {
        zero_count = zero_count + 1
    }
    if (reactance == 0.0) {
        zero_count = zero_count + 1
    }
    if (impedance == 0.0) {
        zero_count = zero_count + 1
    }
    if (zero_count != 1) {
        panic("One and only one argument must be 0")
    }
    if (resistance == 0.0) {
        var value: Double = sqrtApprox((impedance * impedance) - (reactance * reactance))
        return mutableMapOf<String, Double>("resistance" to (value))
    } else {
        if (reactance == 0.0) {
            var value: Double = sqrtApprox((impedance * impedance) - (resistance * resistance))
            return mutableMapOf<String, Double>("reactance" to (value))
        } else {
            if (impedance == 0.0) {
                var value: Double = sqrtApprox((resistance * resistance) + (reactance * reactance))
                return mutableMapOf<String, Double>("impedance" to (value))
            } else {
                panic("Exactly one argument must be 0")
            }
        }
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(electrical_impedance(3.0, 4.0, 0.0))
        println(electrical_impedance(0.0, 4.0, 5.0))
        println(electrical_impedance(3.0, 0.0, 5.0))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
