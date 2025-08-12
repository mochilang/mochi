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

fun exp_approx(x: Double): Double {
    var neg: Boolean = false
    var y: Double = x
    if (x < 0.0) {
        neg = true
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
    if ((neg as Boolean)) {
        return 1.0 / sum
    }
    return sum
}

fun ln_series(x: Double): Double {
    var t: Double = (x - 1.0) / (x + 1.0)
    var term: Double = t
    var acc: Double = 0.0
    var n: Int = (1).toInt()
    while (n <= 19) {
        acc = acc + (term / ((n.toDouble())))
        term = (term * t) * t
        n = n + 2
    }
    return 2.0 * acc
}

fun ln(x: Double): Double {
    var y: Double = x
    var k: Int = (0).toInt()
    while (y >= 10.0) {
        y = y / 10.0
        k = k + 1
    }
    while (y < 1.0) {
        y = y * 10.0
        k = k - 1
    }
    return ln_series(y) + (((k.toDouble())) * ln_series(10.0))
}

fun softplus(x: Double): Double {
    return ln(1.0 + exp_approx(x))
}

fun tanh_approx(x: Double): Double {
    return (2.0 / (1.0 + exp_approx((0.0 - 2.0) * x))) - 1.0
}

fun mish(vector: MutableList<Double>): MutableList<Double> {
    var result: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < vector.size) {
        var x: Double = vector[i]!!
        var sp: Double = softplus(x)
        var y: Double = x * tanh_approx(sp)
        result = run { val _tmp = result.toMutableList(); _tmp.add(y); _tmp }
        i = i + 1
    }
    return result
}

fun user_main(): Unit {
    var v1: MutableList<Double> = mutableListOf(2.3, 0.6, 0.0 - 2.0, 0.0 - 3.8)
    var v2: MutableList<Double> = mutableListOf(0.0 - 9.2, 0.0 - 0.3, 0.45, 0.0 - 4.56)
    println(mish(v1).toString())
    println(mish(v2).toString())
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
