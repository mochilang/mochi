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

var units: MutableList<String> = mutableListOf("km/h", "m/s", "mph", "knot")
var speed_chart: MutableList<Double> = mutableListOf(1.0, 3.6, 1.609344, 1.852)
var speed_chart_inverse: MutableList<Double> = mutableListOf(1.0, 0.277777778, 0.621371192, 0.539956803)
fun index_of(arr: MutableList<String>, value: String): Int {
    var i: Int = 0
    while (i < arr.size) {
        if (arr[i]!! == value) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun units_string(arr: MutableList<String>): String {
    var s: String = ""
    var i: Int = 0
    while (i < arr.size) {
        if (i > 0) {
            s = s + ", "
        }
        s = s + arr[i]!!
        i = i + 1
    }
    return s
}

fun round3(x: Double): Double {
    var y: Double = (x * 1000.0) + 0.5
    var z: Int = (y.toInt())
    var zf: Double = (z.toDouble())
    return zf / 1000.0
}

fun convert_speed(speed: Double, unit_from: String, unit_to: String): Double {
    var from_index: Int = index_of(units, unit_from)
    var to_index: Int = index_of(units, unit_to)
    if ((from_index < 0) || (to_index < 0)) {
        var msg: String = (((("Incorrect 'from_type' or 'to_type' value: " + unit_from) + ", ") + unit_to) + "\nValid values are: ") + units_string(units)
        panic(msg)
    }
    var result: Double = (speed * speed_chart[from_index]!!) * speed_chart_inverse[to_index]!!
    var r: Double = round3(result)
    return r
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(convert_speed(100.0, "km/h", "m/s").toString())
        println(convert_speed(100.0, "km/h", "mph").toString())
        println(convert_speed(100.0, "km/h", "knot").toString())
        println(convert_speed(100.0, "m/s", "km/h").toString())
        println(convert_speed(100.0, "m/s", "mph").toString())
        println(convert_speed(100.0, "m/s", "knot").toString())
        println(convert_speed(100.0, "mph", "km/h").toString())
        println(convert_speed(100.0, "mph", "m/s").toString())
        println(convert_speed(100.0, "mph", "knot").toString())
        println(convert_speed(100.0, "knot", "km/h").toString())
        println(convert_speed(100.0, "knot", "m/s").toString())
        println(convert_speed(100.0, "knot", "mph").toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
