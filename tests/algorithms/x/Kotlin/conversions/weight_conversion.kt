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

var KILOGRAM_CHART: MutableMap<String, Double> = mutableMapOf<String, Double>("kilogram" to (1.0), "gram" to (1000.0), "milligram" to (1000000.0), "metric-ton" to (0.001), "long-ton" to (0.0009842073), "short-ton" to (0.0011023122), "pound" to (2.2046244202), "stone" to (0.1574731728), "ounce" to (35.273990723), "carrat" to (5000.0), "atomic-mass-unit" to (6.022136652 * pow10(26)))
var WEIGHT_TYPE_CHART: MutableMap<String, Double> = mutableMapOf<String, Double>("kilogram" to (1.0), "gram" to (0.001), "milligram" to (0.000001), "metric-ton" to (1000.0), "long-ton" to (1016.04608), "short-ton" to (907.184), "pound" to (0.453592), "stone" to (6.35029), "ounce" to (0.0283495), "carrat" to (0.0002), "atomic-mass-unit" to (1.660540199 * pow10(0 - 27)))
fun pow10(exp: Int): Double {
    var result: Double = 1.0
    if (exp >= 0) {
        var i: Int = 0
        while (i < exp) {
            result = result * 10.0
            i = i + 1
        }
    } else {
        var i: Int = 0
        while (i < (0 - exp)) {
            result = result / 10.0
            i = i + 1
        }
    }
    return result
}

fun weight_conversion(from_type: String, to_type: String, value: Double): Double {
    var has_to: Boolean = to_type in KILOGRAM_CHART
    var has_from: Boolean = from_type in WEIGHT_TYPE_CHART
    if (has_to && has_from) {
        return (value * (KILOGRAM_CHART)[to_type] as Double) * (WEIGHT_TYPE_CHART)[from_type] as Double
    }
    println("Invalid 'from_type' or 'to_type'")
    return 0.0
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(weight_conversion("kilogram", "gram", 1.0))
        println(weight_conversion("gram", "pound", 3.0))
        println(weight_conversion("ounce", "kilogram", 3.0))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
