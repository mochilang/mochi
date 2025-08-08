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

var UNIT_SYMBOL: MutableMap<String, String> = mutableMapOf<String, String>("meter" to ("m"), "kilometer" to ("km"), "megametre" to ("Mm"), "gigametre" to ("Gm"), "terametre" to ("Tm"), "petametre" to ("Pm"), "exametre" to ("Em"), "zettametre" to ("Zm"), "yottametre" to ("Ym"))
var METRIC_CONVERSION: MutableMap<String, Int> = mutableMapOf<String, Int>("m" to (0), "km" to (3), "Mm" to (6), "Gm" to (9), "Tm" to (12), "Pm" to (15), "Em" to (18), "Zm" to (21), "Ym" to (24))
var ABBREVIATIONS: String = "m, km, Mm, Gm, Tm, Pm, Em, Zm, Ym"
fun sanitize(unit: String): String {
    var res: String = (unit.toLowerCase() as String)
    if (res.length > 0) {
        var last: String = res.substring(res.length - 1, res.length)
        if (last == "s") {
            res = res.substring(0, res.length - 1)
        }
    }
    if (res in UNIT_SYMBOL) {
        return (UNIT_SYMBOL)[res] as String
    }
    return res
}

fun pow10(exp: Int): Double {
    if (exp == 0) {
        return 1.0
    }
    var e: Int = exp
    var res: Double = 1.0
    if (e < 0) {
        e = 0 - e
    }
    var i: Int = 0
    while (i < e) {
        res = res * 10.0
        i = i + 1
    }
    if (exp < 0) {
        return 1.0 / res
    }
    return res
}

fun length_conversion(value: Double, from_type: String, to_type: String): Double {
    var from_sanitized: String = sanitize(from_type)
    var to_sanitized: String = sanitize(to_type)
    if (!(from_sanitized in METRIC_CONVERSION)) {
        panic((("Invalid 'from_type' value: '" + from_type) + "'.\nConversion abbreviations are: ") + ABBREVIATIONS)
    }
    if (!(to_sanitized in METRIC_CONVERSION)) {
        panic((("Invalid 'to_type' value: '" + to_type) + "'.\nConversion abbreviations are: ") + ABBREVIATIONS)
    }
    var from_exp: Int = (METRIC_CONVERSION)[from_sanitized] as Int
    var to_exp: Int = (METRIC_CONVERSION)[to_sanitized] as Int
    var exponent: Int = 0
    if (from_exp > to_exp) {
        exponent = from_exp - to_exp
    } else {
        exponent = 0 - (to_exp - from_exp)
    }
    return value * pow10(exponent)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(length_conversion(1.0, "meter", "kilometer").toString())
        println(length_conversion(1.0, "meter", "megametre").toString())
        println(length_conversion(1.0, "gigametre", "meter").toString())
        println(length_conversion(1.0, "terametre", "zettametre").toString())
        println(length_conversion(1.0, "yottametre", "zettametre").toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
