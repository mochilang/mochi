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

fun exp_approx(x: Double): Double {
    var sum: Double = 1.0
    var term: Double = 1.0
    var i: Int = (1).toInt()
    while (i <= 20) {
        term = (term * x) / ((i.toDouble()))
        sum = sum + term
        i = i + 1
    }
    return sum
}

fun sigmoid(vector: MutableList<Double>): MutableList<Double> {
    var result: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < vector.size) {
        var v: Double = vector[i]!!
        var s: Double = 1.0 / (1.0 + exp_approx(0.0 - v))
        result = run { val _tmp = result.toMutableList(); _tmp.add(s); _tmp }
        i = i + 1
    }
    return result
}

fun swish(vector: MutableList<Double>, beta: Double): MutableList<Double> {
    var result: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < vector.size) {
        var v: Double = vector[i]!!
        var s: Double = 1.0 / (1.0 + exp_approx((0.0 - beta) * v))
        result = run { val _tmp = result.toMutableList(); _tmp.add(v * s); _tmp }
        i = i + 1
    }
    return result
}

fun sigmoid_linear_unit(vector: MutableList<Double>): MutableList<Double> {
    return swish(vector, 1.0)
}

fun approx_equal(a: Double, b: Double, eps: Double): Boolean {
    var diff: Double = (if (a > b) a - b else b - a.toDouble())
    return diff < eps
}

fun approx_equal_list(a: MutableList<Double>, b: MutableList<Double>, eps: Double): Boolean {
    if (a.size != b.size) {
        return false
    }
    var i: Int = (0).toInt()
    while (i < a.size) {
        if (!approx_equal(a[i]!!, b[i]!!, eps)) {
            return false
        }
        i = i + 1
    }
    return true
}

fun test_swish(): Unit {
    var v: MutableList<Double> = mutableListOf(0.0 - 1.0, 1.0, 2.0)
    var eps: Double = 0.001
    if (!approx_equal_list(sigmoid(v), mutableListOf(0.26894142, 0.73105858, 0.88079708), eps)) {
        panic("sigmoid incorrect")
    }
    if (!approx_equal_list(sigmoid_linear_unit(v), mutableListOf(0.0 - 0.26894142, 0.73105858, 1.76159416), eps)) {
        panic("sigmoid_linear_unit incorrect")
    }
    if (!approx_equal_list(swish(v, 2.0), mutableListOf(0.0 - 0.11920292, 0.88079708, 1.96402758), eps)) {
        panic("swish incorrect")
    }
    if (!approx_equal_list(swish(mutableListOf(0.0 - 2.0), 1.0), mutableListOf(0.0 - 0.23840584), eps)) {
        panic("swish with parameter 1 incorrect")
    }
}

fun user_main(): Unit {
    test_swish()
    println(sigmoid(mutableListOf(0.0 - 1.0, 1.0, 2.0)).toString())
    println(sigmoid_linear_unit(mutableListOf(0.0 - 1.0, 1.0, 2.0)).toString())
    println(swish(mutableListOf(0.0 - 1.0, 1.0, 2.0), 2.0).toString())
    println(swish(mutableListOf(0.0 - 2.0), 1.0).toString())
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
