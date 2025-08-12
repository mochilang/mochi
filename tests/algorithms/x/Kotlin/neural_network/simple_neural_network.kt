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

var seed: Int = (1).toInt()
var INITIAL_VALUE: Double = 0.02
fun rand(): Int {
    seed = (((Math.floorMod((((seed * 1103515245) + 12345).toLong()), 2147483648L)).toInt())).toInt()
    return seed
}

fun randint(low: Int, high: Int): Int {
    return (Math.floorMod(rand(), ((high - low) + 1))) + low
}

fun expApprox(x: Double): Double {
    var y: Double = x
    var is_neg: Boolean = false
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

fun sigmoid(x: Double): Double {
    return 1.0 / (1.0 + expApprox(0.0 - x))
}

fun sigmoid_derivative(sig_val: Double): Double {
    return sig_val * (1.0 - sig_val)
}

fun forward_propagation(expected: Int, number_propagations: Int): Double {
    var weight: Double = (2.0 * (((randint(1, 100)).toDouble()))) - 1.0
    var layer_1: Double = 0.0
    var i: Int = (0).toInt()
    while (i < number_propagations) {
        layer_1 = sigmoid(INITIAL_VALUE * weight)
        var layer_1_error: Double = (((expected.toDouble())) / 100.0) - layer_1
        var layer_1_delta: Double = layer_1_error * sigmoid_derivative(layer_1)
        weight = weight + (INITIAL_VALUE * layer_1_delta)
        i = i + 1
    }
    return layer_1 * 100.0
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        seed = (1).toInt()
        var result: Double = forward_propagation(32, 450000)
        println(result)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
