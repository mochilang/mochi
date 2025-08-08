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

var time_chart: MutableMap<String, Double> = mutableMapOf<String, Double>("seconds" to (1.0), "minutes" to (60.0), "hours" to (3600.0), "days" to (86400.0), "weeks" to (604800.0), "months" to (2629800.0), "years" to (31557600.0))
var time_chart_inverse: MutableMap<String, Double> = mutableMapOf<String, Double>("seconds" to (1.0), "minutes" to (1.0 / 60.0), "hours" to (1.0 / 3600.0), "days" to (1.0 / 86400.0), "weeks" to (1.0 / 604800.0), "months" to (1.0 / 2629800.0), "years" to (1.0 / 31557600.0))
var units: MutableList<String> = mutableListOf("seconds", "minutes", "hours", "days", "weeks", "months", "years")
var units_str: String = "seconds, minutes, hours, days, weeks, months, years"
fun contains(arr: MutableList<String>, t: String): Boolean {
    var i: Int = 0
    while (i < arr.size) {
        if (arr[i]!! == t) {
            return true
        }
        i = i + 1
    }
    return false
}

fun convert_time(time_value: Double, unit_from: String, unit_to: String): Double {
    if (time_value < 0.0) {
        panic("'time_value' must be a non-negative number.")
    }
    var from: String = (unit_from.toLowerCase() as String)
    var to: String = (unit_to.toLowerCase() as String)
    if ((!((units.contains(from)) as Boolean) as Boolean) || (!((units.contains(to)) as Boolean) as Boolean)) {
        var invalid_unit: String = from
        if (units.contains(from)) {
            invalid_unit = to
        }
        panic(((("Invalid unit " + invalid_unit) + " is not in ") + units_str) + ".")
    }
    var seconds: Double = time_value * (time_chart)[from] as Double
    var converted: Double = seconds * (time_chart_inverse)[to] as Double
    var scaled: Double = converted * 1000.0
    var int_part: Int = ((scaled + 0.5).toInt())
    return (int_part + 0.0) / 1000.0
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(convert_time(3600.0, "seconds", "hours"))
        println(convert_time(360.0, "days", "months"))
        println(convert_time(360.0, "months", "years"))
        println(convert_time(1.0, "years", "seconds"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
