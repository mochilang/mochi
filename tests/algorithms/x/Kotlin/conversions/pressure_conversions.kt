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

data class FromTo(var from_factor: Double = 0.0, var to_factor: Double = 0.0)
var PRESSURE_CONVERSION: MutableMap<String, FromTo> = mutableMapOf<String, FromTo>("atm" to (FromTo(from_factor = 1.0, to_factor = 1.0)), "pascal" to (FromTo(from_factor = 0.0000098, to_factor = 101325.0)), "bar" to (FromTo(from_factor = 0.986923, to_factor = 1.01325)), "kilopascal" to (FromTo(from_factor = 0.00986923, to_factor = 101.325)), "megapascal" to (FromTo(from_factor = 9.86923, to_factor = 0.101325)), "psi" to (FromTo(from_factor = 0.068046, to_factor = 14.6959)), "inHg" to (FromTo(from_factor = 0.0334211, to_factor = 29.9213)), "torr" to (FromTo(from_factor = 0.00131579, to_factor = 760.0)))
fun pressure_conversion(value: Double, from_type: String, to_type: String): Double {
    if (!(from_type in PRESSURE_CONVERSION)) {
        var keys: Any? = PRESSURE_CONVERSION.keys.joinToString(", ")
        panic((("Invalid 'from_type' value: '" + from_type) + "'  Supported values are:\n") + (keys).toString())
    }
    if (!(to_type in PRESSURE_CONVERSION)) {
        var keys: Any? = PRESSURE_CONVERSION.keys.joinToString(", ")
        panic((("Invalid 'to_type' value: '" + to_type) + ".  Supported values are:\n") + (keys).toString())
    }
    var from: FromTo = (PRESSURE_CONVERSION)[from_type] as FromTo
    var to: FromTo = (PRESSURE_CONVERSION)[to_type] as FromTo
    return (value * from.from_factor) * to.to_factor
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(pressure_conversion(4.0, "atm", "pascal"))
        println(pressure_conversion(1.0, "pascal", "psi"))
        println(pressure_conversion(1.0, "bar", "atm"))
        println(pressure_conversion(3.0, "kilopascal", "bar"))
        println(pressure_conversion(2.0, "megapascal", "psi"))
        println(pressure_conversion(4.0, "psi", "torr"))
        println(pressure_conversion(1.0, "inHg", "atm"))
        println(pressure_conversion(1.0, "torr", "psi"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
