import java.math.BigInteger

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

var SI_UNITS: MutableMap<String, Int> = mutableMapOf<String, Int>("yotta" to (24), "zetta" to (21), "exa" to (18), "peta" to (15), "tera" to (12), "giga" to (9), "mega" to (6), "kilo" to (3), "hecto" to (2), "deca" to (1), "deci" to (0 - 1), "centi" to (0 - 2), "milli" to (0 - 3), "micro" to (0 - 6), "nano" to (0 - 9), "pico" to (0 - 12), "femto" to (0 - 15), "atto" to (0 - 18), "zepto" to (0 - 21), "yocto" to (0 - 24))
var BINARY_UNITS: MutableMap<String, Int> = mutableMapOf<String, Int>("yotta" to (8), "zetta" to (7), "exa" to (6), "peta" to (5), "tera" to (4), "giga" to (3), "mega" to (2), "kilo" to (1))
fun pow(base: Double, exp: Int): Double {
    if (exp == 0) {
        return 1.0
    }
    var e: Int = exp
    if (e < 0) {
        e = 0 - e
    }
    var result: Double = 1.0
    var i: Int = 0
    while (i < e) {
        result = result * base
        i = i + 1
    }
    if (exp < 0) {
        return 1.0 / result
    }
    return result
}

fun convert_si_prefix(known_amount: Double, known_prefix: String, unknown_prefix: String): Double {
    var kp: String = (known_prefix.toLowerCase() as String)
    var up: String = (unknown_prefix.toLowerCase() as String)
    if (!(kp in SI_UNITS)) {
        panic("unknown prefix: " + known_prefix)
    }
    if (!(up in SI_UNITS)) {
        panic("unknown prefix: " + unknown_prefix)
    }
    var diff: Int = (SI_UNITS)[kp] as Int - (SI_UNITS)[up] as Int
    return known_amount * pow(10.0, diff)
}

fun convert_binary_prefix(known_amount: Double, known_prefix: String, unknown_prefix: String): Double {
    var kp: String = (known_prefix.toLowerCase() as String)
    var up: String = (unknown_prefix.toLowerCase() as String)
    if (!(kp in BINARY_UNITS)) {
        panic("unknown prefix: " + known_prefix)
    }
    if (!(up in BINARY_UNITS)) {
        panic("unknown prefix: " + unknown_prefix)
    }
    var diff: Int = ((BINARY_UNITS)[kp] as Int - (BINARY_UNITS)[up] as Int) * 10
    return known_amount * pow(2.0, diff)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(convert_si_prefix(1.0, "giga", "mega").toString())
        println(convert_si_prefix(1.0, "mega", "giga").toString())
        println(convert_si_prefix(1.0, "kilo", "kilo").toString())
        println(convert_binary_prefix(1.0, "giga", "mega").toString())
        println(convert_binary_prefix(1.0, "mega", "giga").toString())
        println(convert_binary_prefix(1.0, "kilo", "kilo").toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
