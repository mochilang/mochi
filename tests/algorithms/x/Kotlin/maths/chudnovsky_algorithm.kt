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

var n: Int = (50).toInt()
fun sqrtApprox(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun factorial_float(n: Int): Double {
    var result: Double = 1.0
    var i: Int = (2).toInt()
    while (i <= n) {
        result = result * ((i.toDouble()))
        i = i + 1
    }
    return result
}

fun pi(n: Int): Double {
    if (n < 1) {
        panic("Undefined for non-natural numbers")
    }
    var iterations: Int = ((n + 13) / 14).toInt()
    var constant_term: Double = 426880.0 * sqrtApprox(10005.0)
    var exponential_term: Double = 1.0
    var linear_term: Double = 13591409.0
    var partial_sum: Double = linear_term
    var k: Int = (1).toInt()
    while (k < iterations) {
        var k6: Int = (6 * k).toInt()
        var k3: Int = (3 * k).toInt()
        var fact6k: Double = factorial_float(k6)
        var fact3k: Double = factorial_float(k3)
        var factk: Double = factorial_float(k)
        var multinomial: Double = fact6k / (((fact3k * factk) * factk) * factk)
        linear_term = linear_term + 545140134.0
        exponential_term = exponential_term * (0.0 - 262537412640768000.0)
        partial_sum = partial_sum + ((multinomial * linear_term) / exponential_term)
        k = k + 1
    }
    return constant_term / partial_sum
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println((("The first " + n.toString()) + " digits of pi is: ") + pi(n).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
