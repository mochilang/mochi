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

fun abs(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun sqrtApprox(x: Double): Double {
    var guess: Double = x
    var i: Int = 0
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun agmPi(): Double {
    var a: Double = 1.0
    var g: Double = 1.0 / sqrtApprox(2.0)
    var sum: Double = 0.0
    var pow: Double = 2.0
    while (kotlin.math.abs(a - g) > 0.000000000000001) {
        var t: Double = (a + g) / 2.0
        var u: Double = sqrtApprox(a * g)
        a = t
        g = u
        pow = pow * 2.0
        var diff: Double = (a * a) - (g * g)
        sum = sum + (diff * pow)
    }
    var pi: Double = ((4.0 * a) * a) / (1.0 - sum)
    return pi
}

fun user_main(): Unit {
    println(agmPi().toString())
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
