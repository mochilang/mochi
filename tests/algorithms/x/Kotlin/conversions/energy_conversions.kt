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

var ENERGY_CONVERSION: MutableMap<String, Double> = mutableMapOf<String, Double>("joule" to (1.0), "kilojoule" to (1000.0), "megajoule" to (1000000.0), "gigajoule" to (1000000000.0), "wattsecond" to (1.0), "watthour" to (3600.0), "kilowatthour" to (3600000.0), "newtonmeter" to (1.0), "calorie_nutr" to (4186.8), "kilocalorie_nutr" to (4186800.0), "electronvolt" to (0.0000000000000000001602176634), "britishthermalunit_it" to (1055.05585), "footpound" to (1.355818))
fun energy_conversion(from_type: String, to_type: String, value: Double): Double {
    if (((from_type in ENERGY_CONVERSION) == false) || ((to_type in ENERGY_CONVERSION) == false)) {
        panic("Incorrect 'from_type' or 'to_type'")
    }
    return (value * (ENERGY_CONVERSION)[from_type] as Double) / (ENERGY_CONVERSION)[to_type] as Double
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(energy_conversion("joule", "kilojoule", 1.0).toString())
        println(energy_conversion("kilowatthour", "joule", 10.0).toString())
        println(energy_conversion("britishthermalunit_it", "footpound", 1.0).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
