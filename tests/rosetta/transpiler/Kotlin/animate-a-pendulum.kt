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

val PI: Double = 3.141592653589793
val L: Double = 10.0
val G: Double = 9.81
val dt: Double = 0.2
val phi0: Double = PI / 4.0
val omega: Double = sqrtApprox(G / L)
var t: Double = 0.0
fun sinApprox(x: Double): Double {
    var term: Double = x
    var sum: Double = x
    var n: Int = 1
    while (n <= 10) {
        val denom: Double = ((2 * n) * ((2 * n) + 1)).toDouble()
        term = (((0.0 - term) * x) * x) / denom
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun cosApprox(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = 1
    while (n <= 10) {
        val denom: Double = (((2 * n) - 1) * (2 * n)).toDouble()
        term = (((0.0 - term) * x) * x) / denom
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun sqrtApprox(x: Double): Double {
    var guess: Double = x
    var i: Int = 0
    while (i < 10) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (step in 0 until 10) {
            val phi: Double = phi0 * cosApprox(omega * t)
            val pos: Int = ((10.0 * sinApprox(phi)) + 0.5).toInt()
            println(pos.toString())
            t = t + dt
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
