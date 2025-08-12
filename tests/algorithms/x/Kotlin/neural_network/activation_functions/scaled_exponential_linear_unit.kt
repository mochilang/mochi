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

fun exp(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = (1).toInt()
    while (n < 20) {
        term = (term * x) / ((n.toDouble()))
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun scaled_exponential_linear_unit(vector: MutableList<Double>, alpha: Double, lambda_: Double): MutableList<Double> {
    var result: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < vector.size) {
        var x: Double = vector[i]!!
        var y: Double = (if (x > 0.0) lambda_ * x else (lambda_ * alpha) * (exp(x) - 1.0).toDouble())
        result = run { val _tmp = result.toMutableList(); _tmp.add(y); _tmp }
        i = i + 1
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(scaled_exponential_linear_unit(mutableListOf(1.3, 3.7, 2.4), 1.6732, 1.0507))
        println(scaled_exponential_linear_unit(mutableListOf(1.3, 4.7, 8.2), 1.6732, 1.0507))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
