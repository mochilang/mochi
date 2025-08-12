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

var sample: MutableList<Double> = mutableListOf(0.0 - 1.0, 1.0, 2.0)
fun exp_taylor(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var i: Double = 1.0
    while (i < 20.0) {
        term = (term * x) / i
        sum = sum + term
        i = i + 1.0
    }
    return sum
}

fun sigmoid(vector: MutableList<Double>): MutableList<Double> {
    var result: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < vector.size) {
        var x: Double = vector[i]!!
        var value: Double = 1.0 / (1.0 + exp_taylor(0.0 - x))
        result = run { val _tmp = result.toMutableList(); _tmp.add(value); _tmp }
        i = i + 1
    }
    return result
}

fun gaussian_error_linear_unit(vector: MutableList<Double>): MutableList<Double> {
    var result: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < vector.size) {
        var x: Double = vector[i]!!
        var gelu: Double = x * (1.0 / (1.0 + exp_taylor((0.0 - 1.702) * x)))
        result = run { val _tmp = result.toMutableList(); _tmp.add(gelu); _tmp }
        i = i + 1
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(sigmoid(sample))
        println(gaussian_error_linear_unit(sample))
        println(gaussian_error_linear_unit(mutableListOf(0.0 - 3.0)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
