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
    var sum: Double = 1.0
    var term: Double = 1.0
    var i: Int = (1).toInt()
    var absx: Double = (if (x < 0.0) 0.0 - x else x.toDouble())
    while (i <= 20) {
        term = (term * absx) / ((i.toDouble()))
        sum = sum + term
        i = i + 1
    }
    if (x < 0.0) {
        return 1.0 / sum
    }
    return sum
}

fun exponential_linear_unit(vector: MutableList<Double>, alpha: Double): MutableList<Double> {
    var result: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < vector.size) {
        var v: Double = vector[i]!!
        if (v > 0.0) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(v); _tmp }
        } else {
            var neg: Double = alpha * (exp_approx(v) - 1.0)
            result = run { val _tmp = result.toMutableList(); _tmp.add(neg); _tmp }
        }
        i = i + 1
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(exponential_linear_unit(mutableListOf(2.3, 0.6, 0.0 - 2.0, 0.0 - 3.8), 0.3).toString())
        println(exponential_linear_unit(mutableListOf(0.0 - 9.2, 0.0 - 0.3, 0.45, 0.0 - 4.56), 0.067).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
