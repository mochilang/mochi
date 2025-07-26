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

var e1: Int = pow_int(3, 2)
var e2: Int = pow_int(4, e1)
var base: BigInteger = java.math.BigInteger.valueOf(5)
var x: BigInteger = pow_big(base, e2)
var s: String = x.toString()
fun pow_int(base: Int, exp: Int): Int {
    var result: Int = 1
    var b: Int = base
    var e: Int = exp
    while (e > 0) {
        if ((e % 2) == 1) {
            result = result * b
        }
        b = b * b
        e = (e / 2).toInt()
    }
    return result
}

fun pow_big(base: BigInteger, exp: Int): BigInteger {
    var result: BigInteger = java.math.BigInteger.valueOf(1)
    var b: BigInteger = base
    var e: Int = exp
    while (e > 0) {
        if ((e % 2) == 1) {
            result = result.multiply(b)
        }
        b = b.multiply(b)
        e = (e / 2).toInt()
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(listOf("5^(4^(3^2)) has", s.length, "digits:", s.substring(0, 20), "...", s.substring(s.length - 20, s.length)).joinToString(" "))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
