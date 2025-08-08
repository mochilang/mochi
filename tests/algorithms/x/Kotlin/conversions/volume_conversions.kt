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

var units: MutableList<String> = mutableListOf("cubic meter", "litre", "kilolitre", "gallon", "cubic yard", "cubic foot", "cup")
var from_factors: MutableList<Double> = mutableListOf(1.0, 0.001, 1.0, 0.00454, 0.76455, 0.028, 0.000236588)
var to_factors: MutableList<Double> = mutableListOf(1.0, 1000.0, 1.0, 264.172, 1.30795, 35.3147, 4226.75)
fun supported_values(): String {
    var result: String = units[0]!!
    var i: Int = 1
    while (i < units.size) {
        result = (result + ", ") + units[i]!!
        i = i + 1
    }
    return result
}

fun find_index(name: String): Int {
    var i: Int = 0
    while (i < units.size) {
        if (units[i]!! == name) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun get_from_factor(name: String): Double {
    var idx: Int = find_index(name)
    if (idx < 0) {
        panic((("Invalid 'from_type' value: '" + name) + "' Supported values are: ") + supported_values())
    }
    return from_factors[idx]!!
}

fun get_to_factor(name: String): Double {
    var idx: Int = find_index(name)
    if (idx < 0) {
        panic((("Invalid 'to_type' value: '" + name) + "' Supported values are: ") + supported_values())
    }
    return to_factors[idx]!!
}

fun volume_conversion(value: Double, from_type: String, to_type: String): Double {
    var from_factor: Double = get_from_factor(from_type)
    var to_factor: Double = get_to_factor(to_type)
    return (value * from_factor) * to_factor
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(volume_conversion(4.0, "cubic meter", "litre").toString())
        println(volume_conversion(1.0, "litre", "gallon").toString())
        println(volume_conversion(1.0, "kilolitre", "cubic meter").toString())
        println(volume_conversion(3.0, "gallon", "cubic yard").toString())
        println(volume_conversion(2.0, "cubic yard", "litre").toString())
        println(volume_conversion(4.0, "cubic foot", "cup").toString())
        println(volume_conversion(1.0, "cup", "kilolitre").toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
