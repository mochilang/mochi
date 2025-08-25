val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/dynamic_programming"

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
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

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

fun maximum_non_adjacent_sum(nums: MutableList<Int>): Int {
    if (nums.size == 0) {
        return 0
    }
    var max_including: Int = (nums[0]!!).toInt()
    var max_excluding: Int = (0).toInt()
    var i: Int = (1).toInt()
    while (i < nums.size) {
        var num: Int = (nums[i]!!).toInt()
        var new_including: Int = (max_excluding + num).toInt()
        var new_excluding: Int = (if (max_including > max_excluding) max_including else max_excluding).toInt()
        max_including = new_including
        max_excluding = new_excluding
        i = i + 1
    }
    if (max_including > max_excluding) {
        return max_including
    }
    return max_excluding
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(maximum_non_adjacent_sum(mutableListOf(1, 2, 3))))
        println(_numToStr(maximum_non_adjacent_sum(mutableListOf(1, 5, 3, 7, 2, 2, 6))))
        println(_numToStr(maximum_non_adjacent_sum(mutableListOf(0 - 1, 0 - 5, 0 - 3, 0 - 7, 0 - 2, 0 - 2, 0 - 6))))
        println(_numToStr(maximum_non_adjacent_sum(mutableListOf(499, 500, 0 - 3, 0 - 7, 0 - 2, 0 - 2, 0 - 6))))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
