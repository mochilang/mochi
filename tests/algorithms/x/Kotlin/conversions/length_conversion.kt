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

fun rstrip_s(s: String): String {
    if ((s.length > 0) && (s[s.length - 1].toString() == "s")) {
        return s.substring(0, s.length - 1)
    }
    return s
}

fun normalize_alias(u: String): String {
    if (u == "millimeter") {
        return "mm"
    }
    if (u == "centimeter") {
        return "cm"
    }
    if (u == "meter") {
        return "m"
    }
    if (u == "kilometer") {
        return "km"
    }
    if (u == "inch") {
        return "in"
    }
    if (u == "inche") {
        return "in"
    }
    if (u == "feet") {
        return "ft"
    }
    if (u == "foot") {
        return "ft"
    }
    if (u == "yard") {
        return "yd"
    }
    if (u == "mile") {
        return "mi"
    }
    return u
}

fun has_unit(u: String): Boolean {
    return (((((((((((((((u == "mm") || (u == "cm") as Boolean)) || (u == "m") as Boolean)) || (u == "km") as Boolean)) || (u == "in") as Boolean)) || (u == "ft") as Boolean)) || (u == "yd") as Boolean)) || (u == "mi")) as Boolean)
}

fun from_factor(u: String): Double {
    if (u == "mm") {
        return 0.001
    }
    if (u == "cm") {
        return 0.01
    }
    if (u == "m") {
        return 1.0
    }
    if (u == "km") {
        return 1000.0
    }
    if (u == "in") {
        return 0.0254
    }
    if (u == "ft") {
        return 0.3048
    }
    if (u == "yd") {
        return 0.9144
    }
    if (u == "mi") {
        return 1609.34
    }
    return 0.0
}

fun to_factor(u: String): Double {
    if (u == "mm") {
        return 1000.0
    }
    if (u == "cm") {
        return 100.0
    }
    if (u == "m") {
        return 1.0
    }
    if (u == "km") {
        return 0.001
    }
    if (u == "in") {
        return 39.3701
    }
    if (u == "ft") {
        return 3.28084
    }
    if (u == "yd") {
        return 1.09361
    }
    if (u == "mi") {
        return 0.000621371
    }
    return 0.0
}

fun length_conversion(value: Double, from_type: String, to_type: String): Double {
    var new_from: String = normalize_alias(rstrip_s((from_type.toLowerCase() as String)))
    var new_to: String = normalize_alias(rstrip_s((to_type.toLowerCase() as String)))
    if (!has_unit(new_from)) {
        panic(("Invalid 'from_type' value: '" + from_type) + "'.\nConversion abbreviations are: mm, cm, m, km, in, ft, yd, mi")
    }
    if (!has_unit(new_to)) {
        panic(("Invalid 'to_type' value: '" + to_type) + "'.\nConversion abbreviations are: mm, cm, m, km, in, ft, yd, mi")
    }
    return (value * from_factor(new_from)) * to_factor(new_to)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(length_conversion(4.0, "METER", "FEET"))
        println(length_conversion(1.0, "kilometer", "inch"))
        println(length_conversion(2.0, "feet", "meter"))
        println(length_conversion(2.0, "centimeter", "millimeter"))
        println(length_conversion(4.0, "yard", "kilometer"))
        println(length_conversion(3.0, "foot", "inch"))
        println(length_conversion(3.0, "mm", "in"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
