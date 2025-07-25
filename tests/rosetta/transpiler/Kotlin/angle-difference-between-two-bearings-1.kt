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

var testCases: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(20.0, 45.0), mutableListOf(0 - 45.0, 45.0), mutableListOf(0 - 85.0, 90.0), mutableListOf(0 - 95.0, 90.0), mutableListOf(0 - 45.0, 125.0), mutableListOf(0 - 45.0, 145.0), mutableListOf(29.4803, 0 - 88.6381), mutableListOf(0 - 78.3251, 0 - 159.036))
fun angleDiff(b1: Double, b2: Double): Double {
    val d: Double = b2 - b1
    if (d < (0 - 180.0)) {
        return d + 360.0
    }
    if (d > 180.0) {
        return d - 360.0
    }
    return d
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (tc in testCases) {
            println(angleDiff(tc[0], tc[1]))
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
