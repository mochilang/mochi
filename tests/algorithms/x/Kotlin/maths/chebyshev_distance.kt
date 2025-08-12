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

fun abs(x: Double): Double {
    if (x >= 0.0) {
        return x
    } else {
        return 0.0 - x
    }
}

fun chebyshev_distance(point_a: MutableList<Double>, point_b: MutableList<Double>): Double {
    if (point_a.size != point_b.size) {
        panic("Both points must have the same dimension.")
    }
    var max_diff: Double = 0.0
    var i: Int = (0).toInt()
    while (i < point_a.size) {
        var diff: Double = kotlin.math.abs(point_a[i]!! - point_b[i]!!)
        if (diff > max_diff) {
            max_diff = diff
        }
        i = i + 1
    }
    return max_diff
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(chebyshev_distance(mutableListOf(1.0, 1.0), mutableListOf(2.0, 2.0)))
        println(chebyshev_distance(mutableListOf(1.0, 1.0, 9.0), mutableListOf(2.0, 2.0, 0.0 - 5.2)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
