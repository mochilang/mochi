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

fun abs_float(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun average_absolute_deviation(nums: MutableList<Int>): Double {
    if (nums.size == 0) {
        panic("List is empty")
    }
    var sum: Int = (0).toInt()
    for (x in nums) {
        sum = sum + x
    }
    var n: Double = (nums.size.toDouble())
    var mean: Double = ((sum.toDouble())) / n
    var dev_sum: Double = 0.0
    for (x in nums) {
        dev_sum = dev_sum + abs_float(((x.toDouble())) - mean)
    }
    return dev_sum / n
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(average_absolute_deviation(mutableListOf(0)).toString())
        println(average_absolute_deviation(mutableListOf(4, 1, 3, 2)).toString())
        println(average_absolute_deviation(mutableListOf(2, 70, 6, 50, 20, 8, 4, 0)).toString())
        println(average_absolute_deviation(mutableListOf(0 - 20, 0, 30, 15)).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
