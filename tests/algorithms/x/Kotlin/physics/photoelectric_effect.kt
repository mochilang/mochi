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

var PLANCK_CONSTANT_JS: Double = 6.6261 / pow10(34)
var PLANCK_CONSTANT_EVS: Double = 4.1357 / pow10(15)
fun pow10(exp: Int): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < exp) {
        result = result * 10.0
        i = i + 1
    }
    return result
}

fun maximum_kinetic_energy(frequency: Double, work_function: Double, in_ev: Boolean): Double {
    if (frequency < 0.0) {
        panic("Frequency can't be negative.")
    }
    var energy: Double = (if (in_ev != null) (PLANCK_CONSTANT_EVS * frequency) - work_function else (PLANCK_CONSTANT_JS * frequency) - work_function.toDouble())
    if (energy > 0.0) {
        return energy
    }
    return 0.0
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(maximum_kinetic_energy(1000000.0, 2.0, false).toString())
        println(maximum_kinetic_energy(1000000.0, 2.0, true).toString())
        println(maximum_kinetic_energy(10000000000000000.0, 2.0, true).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
