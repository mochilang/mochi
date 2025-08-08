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

data class Prefix(var name: String = "", var exp: Int = 0)
var si_positive: MutableList<Prefix> = mutableListOf(Prefix(name = "yotta", exp = 24), Prefix(name = "zetta", exp = 21), Prefix(name = "exa", exp = 18), Prefix(name = "peta", exp = 15), Prefix(name = "tera", exp = 12), Prefix(name = "giga", exp = 9), Prefix(name = "mega", exp = 6), Prefix(name = "kilo", exp = 3), Prefix(name = "hecto", exp = 2), Prefix(name = "deca", exp = 1))
var si_negative: MutableList<Prefix> = mutableListOf(Prefix(name = "deci", exp = 0 - 1), Prefix(name = "centi", exp = 0 - 2), Prefix(name = "milli", exp = 0 - 3), Prefix(name = "micro", exp = 0 - 6), Prefix(name = "nano", exp = 0 - 9), Prefix(name = "pico", exp = 0 - 12), Prefix(name = "femto", exp = 0 - 15), Prefix(name = "atto", exp = 0 - 18), Prefix(name = "zepto", exp = 0 - 21), Prefix(name = "yocto", exp = 0 - 24))
var binary_prefixes: MutableList<Prefix> = mutableListOf(Prefix(name = "yotta", exp = 80), Prefix(name = "zetta", exp = 70), Prefix(name = "exa", exp = 60), Prefix(name = "peta", exp = 50), Prefix(name = "tera", exp = 40), Prefix(name = "giga", exp = 30), Prefix(name = "mega", exp = 20), Prefix(name = "kilo", exp = 10))
fun pow(base: Double, exp: Int): Double {
    var result: Double = 1.0
    var e: Int = exp
    if (e < 0) {
        e = 0 - e
        var i: Int = 0
        while (i < e) {
            result = result * base
            i = i + 1
        }
        return 1.0 / result
    }
    var i: Int = 0
    while (i < e) {
        result = result * base
        i = i + 1
    }
    return result
}

fun add_si_prefix(value: Double): String {
    var prefixes: MutableList<Prefix> = mutableListOf<Prefix>()
    if (value > 0.0) {
        prefixes = si_positive
    } else {
        prefixes = si_negative
    }
    var i: Int = 0
    while (i < prefixes.size) {
        var p: Prefix = prefixes[i]!!
        var num: Double = value / pow(10.0, p.exp)
        if (num > 1.0) {
            return (num.toString() + " ") + p.name
        }
        i = i + 1
    }
    return value.toString()
}

fun add_binary_prefix(value: Double): String {
    var i: Int = 0
    while (i < binary_prefixes.size) {
        var p: Prefix = binary_prefixes[i]!!
        var num: Double = value / pow(2.0, p.exp)
        if (num > 1.0) {
            return (num.toString() + " ") + p.name
        }
        i = i + 1
    }
    return value.toString()
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(add_si_prefix(10000.0))
        println(add_si_prefix(0.005))
        println(add_binary_prefix(65536.0))
        println(add_binary_prefix(512.0))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
