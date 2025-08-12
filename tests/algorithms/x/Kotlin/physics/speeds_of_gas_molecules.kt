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
var R: Double = 8.31446261815324
fun sqrt(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun avg_speed_of_molecule(temperature: Double, molar_mass: Double): Double {
    if (temperature < 0.0) {
        panic("Absolute temperature cannot be less than 0 K")
    }
    if (molar_mass <= 0.0) {
        panic("Molar mass should be greater than 0 kg/mol")
    }
    var expr: Double = ((8.0 * R) * temperature) / (PI * molar_mass)
    var s: Double = sqrt(expr)
    return s
}

fun mps_speed_of_molecule(temperature: Double, molar_mass: Double): Double {
    if (temperature < 0.0) {
        panic("Absolute temperature cannot be less than 0 K")
    }
    if (molar_mass <= 0.0) {
        panic("Molar mass should be greater than 0 kg/mol")
    }
    var expr: Double = ((2.0 * R) * temperature) / molar_mass
    var s: Double = sqrt(expr)
    return s
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(avg_speed_of_molecule(273.0, 0.028).toString())
        println(avg_speed_of_molecule(300.0, 0.032).toString())
        println(mps_speed_of_molecule(273.0, 0.028).toString())
        println(mps_speed_of_molecule(300.0, 0.032).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
