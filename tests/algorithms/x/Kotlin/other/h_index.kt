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

fun subarray(xs: MutableList<Int>, start: Int, end: Int): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var k: Int = (start).toInt()
    while (k < end) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(xs[k]!!); _tmp }
        k = k + 1
    }
    return result
}

fun merge(left_half: MutableList<Int>, right_half: MutableList<Int>): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    var j: Int = (0).toInt()
    while ((i < left_half.size) && (j < right_half.size)) {
        if (left_half[i]!! < right_half[j]!!) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(left_half[i]!!); _tmp }
            i = i + 1
        } else {
            result = run { val _tmp = result.toMutableList(); _tmp.add(right_half[j]!!); _tmp }
            j = j + 1
        }
    }
    while (i < left_half.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(left_half[i]!!); _tmp }
        i = i + 1
    }
    while (j < right_half.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(right_half[j]!!); _tmp }
        j = j + 1
    }
    return result
}

fun merge_sort(array: MutableList<Int>): MutableList<Int> {
    if (array.size <= 1) {
        return array
    }
    var middle: Int = (array.size / 2).toInt()
    var left_half: MutableList<Int> = subarray(array, 0, middle)
    var right_half: MutableList<Int> = subarray(array, middle, array.size)
    var sorted_left: MutableList<Int> = merge_sort(left_half)
    var sorted_right: MutableList<Int> = merge_sort(right_half)
    return merge(sorted_left, sorted_right)
}

fun h_index(citations: MutableList<Int>): Int {
    var idx: Int = (0).toInt()
    while (idx < citations.size) {
        if (citations[idx]!! < 0) {
            panic("The citations should be a list of non negative integers.")
        }
        idx = idx + 1
    }
    var sorted: MutableList<Int> = merge_sort(citations)
    var n: Int = (sorted.size).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        if (sorted[(n - 1) - i]!! <= i) {
            return i
        }
        i = i + 1
    }
    return n
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(h_index(mutableListOf(3, 0, 6, 1, 5)).toString())
        println(h_index(mutableListOf(1, 3, 1)).toString())
        println(h_index(mutableListOf(1, 2, 3)).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
