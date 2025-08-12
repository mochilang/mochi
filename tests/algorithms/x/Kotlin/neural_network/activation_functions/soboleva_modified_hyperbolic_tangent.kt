fun json(v: Any?) { println(toJson(v)) }

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

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

fun soboleva_modified_hyperbolic_tangent(vector: MutableList<Double>, a_value: Double, b_value: Double, c_value: Double, d_value: Double): MutableList<Double> {
    var result: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < vector.size) {
        var x: Double = vector[i]!!
        var numerator: Double = exp(a_value * x) - exp((0.0 - b_value) * x)
        var denominator: Double = exp(c_value * x) + exp((0.0 - d_value) * x)
        result = run { val _tmp = result.toMutableList(); _tmp.add(numerator / denominator); _tmp }
        i = i + 1
    }
    return result
}

fun user_main(): Unit {
    var vector: MutableList<Double> = mutableListOf(5.4, 0.0 - 2.4, 6.3, 0.0 - 5.23, 3.27, 0.56)
    var res: MutableList<Double> = soboleva_modified_hyperbolic_tangent(vector, 0.2, 0.4, 0.6, 0.8)
    json((res as Any?))
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
