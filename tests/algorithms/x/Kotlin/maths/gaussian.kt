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

var PI: Double = 3.141592653589793
fun sqrtApprox(x: Double): Double {
    var guess: Double = x / 2.0
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun expApprox(x: Double): Double {
    var is_neg: Boolean = false
    var y: Double = x
    if (x < 0.0) {
        is_neg = true
        y = 0.0 - x
    }
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = (1).toInt()
    while (n < 30) {
        term = (term * y) / ((n.toDouble()))
        sum = sum + term
        n = n + 1
    }
    if ((is_neg as Boolean)) {
        return 1.0 / sum
    }
    return sum
}

fun gaussian(x: Double, mu: Double, sigma: Double): Double {
    var coeff: Double = 1.0 / sqrtApprox(((2.0 * PI) * sigma) * sigma)
    var exponent: Double = (0.0 - ((x - mu) * (x - mu))) / ((2.0 * sigma) * sigma)
    return coeff * expApprox(exponent)
}

fun user_main(): Unit {
    var result: Double = gaussian(1.0, 0.0, 1.0)
    println(result)
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
