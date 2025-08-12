fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }

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

fun factorial(n: Int): Int {
    if (n < 0) {
        panic("factorial is undefined for negative numbers")
    }
    var result: Int = (1).toInt()
    var i: Int = (2).toInt()
    while (i <= n) {
        result = result * i
        i = i + 1
    }
    return result
}

fun pow_float(base: Double, exp: Int): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun binomial_distribution(successes: Int, trials: Int, prob: Double): Double {
    if (successes > trials) {
        panic("successes must be lower or equal to trials")
    }
    if ((trials < 0) || (successes < 0)) {
        panic("the function is defined for non-negative integers")
    }
    if (!(((0.0 < prob) && (prob < 1.0)) as Boolean)) {
        panic("prob has to be in range of 1 - 0")
    }
    var probability: Double = pow_float(prob, successes) * pow_float(1.0 - prob, trials - successes)
    var numerator: Double = ((factorial(trials)).toDouble())
    var denominator: Double = ((factorial(successes) * factorial(trials - successes)).toDouble())
    var coefficient: Double = numerator / denominator
    return probability * coefficient
}

fun test_example1(): Unit {
    var result: Double = binomial_distribution(3, 5, 0.7)
    expect(kotlin.math.abs(result - 0.3087) < 0.0000001)
}

fun test_example2(): Unit {
    var result: Double = binomial_distribution(2, 4, 0.5)
    expect(kotlin.math.abs(result - 0.375) < 0.0000001)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        test_example1()
        test_example2()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
