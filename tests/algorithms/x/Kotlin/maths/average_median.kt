fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

fun bubble_sort(nums: MutableList<Int>): MutableList<Int> {
    var arr: MutableList<Int> = nums
    var n: Int = (arr.size).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        var j: Int = (0).toInt()
        while (j < (n - 1)) {
            var a: Int = (arr[j]!!).toInt()
            var b: Int = (arr[j + 1]!!).toInt()
            if (a > b) {
                _listSet(arr, j, b)
                _listSet(arr, j + 1, a)
            }
            j = j + 1
        }
        i = i + 1
    }
    return arr
}

fun median(nums: MutableList<Int>): Double {
    var sorted_list: MutableList<Int> = bubble_sort(nums)
    var length: Int = (sorted_list.size).toInt()
    var mid_index: Int = (length / 2).toInt()
    if ((Math.floorMod(length, 2)) == 0) {
        return (((sorted_list[mid_index]!! + sorted_list[mid_index - 1]!!).toDouble())) / 2.0
    } else {
        return ((sorted_list[mid_index]!!).toDouble())
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(median(mutableListOf(0)).toString())
        println(median(mutableListOf(4, 1, 3, 2)).toString())
        println(median(mutableListOf(2, 70, 6, 50, 20, 8, 4)).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
