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

var UNIVERSAL_GAS_CONSTANT: Double = 8.3144598
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

fun rms_speed_of_molecule(temperature: Double, molar_mass: Double): Double {
    if (temperature < 0.0) {
        panic("Temperature cannot be less than 0 K")
    }
    if (molar_mass <= 0.0) {
        panic("Molar mass cannot be less than or equal to 0 kg/mol")
    }
    var num: Double = (3.0 * UNIVERSAL_GAS_CONSTANT) * temperature
    var _val: Double = num / molar_mass
    var result: Double = sqrt(_val)
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("rms_speed_of_molecule(100, 2) = " + rms_speed_of_molecule(100.0, 2.0).toString())
        println("rms_speed_of_molecule(273, 12) = " + rms_speed_of_molecule(273.0, 12.0).toString())
        var vrms: Double = rms_speed_of_molecule(300.0, 28.0)
        println(("Vrms of Nitrogen gas at 300 K is " + vrms.toString()) + " m/s")
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
