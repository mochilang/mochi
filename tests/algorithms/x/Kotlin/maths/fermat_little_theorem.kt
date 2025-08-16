import java.math.BigInteger

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

var p: Int = (701).toInt()
var a: Int = (1000000000).toInt()
var b: Int = (10).toInt()
var left: Int = (Math.floorMod((a / b), p)).toInt()
var right_fast: Int = (Math.floorMod((a * binary_exponentiation(b, p - 2, p)), p)).toInt()
fun binary_exponentiation(a: Int, n: Int, mod: Int): Int {
    if (n == 0) {
        return 1
    }
    if ((Math.floorMod(n, 2)) == 1) {
        return Math.floorMod((binary_exponentiation(a, n - 1, mod) * a), mod)
    }
    var b: Int = (binary_exponentiation(a, n / 2, mod)).toInt()
    return Math.floorMod((b * b), mod)
}

fun naive_exponent_mod(a: Int, n: Int, mod: Int): Int {
    var result: Int = (1).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        result = Math.floorMod((result * a), mod)
        i = i + 1
    }
    return result
}

fun print_bool(b: Boolean): Unit {
    if ((b as Boolean)) {
        println(true)
    } else {
        println(false)
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        print_bool(left.compareTo((right_fast)) == 0)
        var right_naive: Int = (Math.floorMod((a * naive_exponent_mod(b, p - 2, p)), p)).toInt()
        print_bool(left.compareTo((right_naive)) == 0)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
