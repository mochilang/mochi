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

fun round_to_int(x: Double): Int {
    if (x >= 0.0) {
        return ((x + 0.5).toInt())
    }
    return ((x - 0.5).toInt())
}

fun molarity_to_normality(nfactor: Double, moles: Double, volume: Double): Int {
    return round_to_int((moles / volume) * nfactor)
}

fun moles_to_pressure(volume: Double, moles: Double, temperature: Double): Int {
    return round_to_int(((moles * 0.0821) * temperature) / volume)
}

fun moles_to_volume(pressure: Double, moles: Double, temperature: Double): Int {
    return round_to_int(((moles * 0.0821) * temperature) / pressure)
}

fun pressure_and_volume_to_temperature(pressure: Double, moles: Double, volume: Double): Int {
    return round_to_int((pressure * volume) / (0.0821 * moles))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(molarity_to_normality(2.0, 3.1, 0.31).toString())
        println(molarity_to_normality(4.0, 11.4, 5.7).toString())
        println(moles_to_pressure(0.82, 3.0, 300.0).toString())
        println(moles_to_pressure(8.2, 5.0, 200.0).toString())
        println(moles_to_volume(0.82, 3.0, 300.0).toString())
        println(moles_to_volume(8.2, 5.0, 200.0).toString())
        println(pressure_and_volume_to_temperature(0.82, 1.0, 2.0).toString())
        println(pressure_and_volume_to_temperature(8.2, 5.0, 3.0).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
