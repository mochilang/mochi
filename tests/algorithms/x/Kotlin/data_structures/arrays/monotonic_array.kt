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

fun is_monotonic(nums: MutableList<Int>): Boolean {
    if (nums.size <= 2) {
        return true
    }
    var increasing: Boolean = true
    var decreasing: Boolean = true
    var i: Int = (0).toInt()
    while (i < (nums.size - 1)) {
        if (nums[i]!! > nums[i + 1]!!) {
            increasing = false
        }
        if (nums[i]!! < nums[i + 1]!!) {
            decreasing = false
        }
        i = i + 1
    }
    return ((increasing || decreasing) as Boolean)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(is_monotonic(mutableListOf(1, 2, 2, 3)).toString())
        println(is_monotonic(mutableListOf(6, 5, 4, 4)).toString())
        println(is_monotonic(mutableListOf(1, 3, 2)).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
