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

fun abs_val(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
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

fun nth_root(value: Double, n: Int): Double {
    if (value == 0.0) {
        return 0.0
    }
    var x: Double = value / (n.toDouble())
    var i: Int = (0).toInt()
    while (i < 20) {
        var num: Double = (((n - 1).toDouble()) * x) + (value / pow_float(x, n - 1))
        x = num / (n.toDouble())
        i = i + 1
    }
    return x
}

fun minkowski_distance(point_a: MutableList<Double>, point_b: MutableList<Double>, order: Int): Double {
    if (order < 1) {
        panic("The order must be greater than or equal to 1.")
    }
    if (point_a.size != point_b.size) {
        panic("Both points must have the same dimension.")
    }
    var total: Double = 0.0
    var idx: Int = (0).toInt()
    while (idx < point_a.size) {
        var diff: Double = abs_val(point_a[idx]!! - point_b[idx]!!)
        total = total + pow_float(diff, order)
        idx = idx + 1
    }
    return nth_root(total, order)
}

fun test_minkowski(): Unit {
    if (abs_val(minkowski_distance(mutableListOf(1.0, 1.0), mutableListOf(2.0, 2.0), 1) - 2.0) > 0.0001) {
        panic("minkowski_distance test1 failed")
    }
    if (abs_val(minkowski_distance(mutableListOf(1.0, 2.0, 3.0, 4.0), mutableListOf(5.0, 6.0, 7.0, 8.0), 2) - 8.0) > 0.0001) {
        panic("minkowski_distance test2 failed")
    }
}

fun user_main(): Unit {
    test_minkowski()
    println(minkowski_distance(mutableListOf(5.0), mutableListOf(0.0), 3))
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
