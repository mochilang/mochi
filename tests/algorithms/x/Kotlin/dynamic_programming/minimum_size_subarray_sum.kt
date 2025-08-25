import java.math.BigInteger

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

fun minimum_subarray_sum(target: Int, numbers: MutableList<Int>): Int {
    var n: Int = (numbers.size).toInt()
    if (n == 0) {
        return 0
    }
    if (target == 0) {
        var i: Int = (0).toInt()
        while (i < n) {
            if (numbers[i]!! == 0) {
                return 0
            }
            i = i + 1
        }
    }
    var left: Int = (0).toInt()
    var right: Int = (0).toInt()
    var curr_sum: Int = (0).toInt()
    var min_len: Int = (n + 1).toInt()
    while (right < n) {
        curr_sum = curr_sum + numbers[right]!!
        while ((curr_sum >= target) && (left <= right)) {
            var current_len: Int = ((right - left) + 1).toInt()
            if (current_len < min_len) {
                min_len = current_len
            }
            curr_sum = curr_sum - numbers[left]!!
            left = left + 1
        }
        right = right + 1
    }
    if (min_len == (n + 1)) {
        return 0
    }
    return min_len
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(minimum_subarray_sum(7, mutableListOf(2, 3, 1, 2, 4, 3))))
        println(_numToStr(minimum_subarray_sum(7, mutableListOf(2, 3, 0 - 1, 2, 4, 0 - 3))))
        println(_numToStr(minimum_subarray_sum(11, mutableListOf(1, 1, 1, 1, 1, 1, 1, 1))))
        println(_numToStr(minimum_subarray_sum(0, mutableListOf(1, 2, 3))))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
