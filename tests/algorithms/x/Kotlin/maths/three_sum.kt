import java.math.BigInteger

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
            if (arr[j]!! > arr[j + 1]!!) {
                var temp: Int = (arr[j]!!).toInt()
                _listSet(arr, j, arr[j + 1]!!)
                _listSet(arr, j + 1, temp)
            }
            j = j + 1
        }
        i = i + 1
    }
    return arr
}

fun three_sum(nums: MutableList<Int>): MutableList<MutableList<Int>> {
    var sorted: MutableList<Int> = bubble_sort(nums)
    var res: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var n: Int = (sorted.size).toInt()
    var i: Int = (0).toInt()
    while (i < (n - 2)) {
        if ((i == 0) || (sorted[i]!! != sorted[i - 1]!!)) {
            var low: Int = (i + 1).toInt()
            var high: Int = (n - 1).toInt()
            var c: Int = (0 - sorted[i]!!).toInt()
            while (low < high) {
                var s: Int = (sorted[low]!! + sorted[high]!!).toInt()
                if (s == c) {
                    var triple: MutableList<Int> = mutableListOf(sorted[i]!!, sorted[low]!!, sorted[high]!!)
                    res = run { val _tmp = res.toMutableList(); _tmp.add(triple); _tmp }
                    while ((low < high) && (sorted[low]!! == sorted[low + 1]!!)) {
                        low = low + 1
                    }
                    while ((low < high) && (sorted[high]!! == sorted[high - 1]!!)) {
                        high = high - 1
                    }
                    low = low + 1
                    high = high - 1
                } else {
                    if (s < c) {
                        low = low + 1
                    } else {
                        high = high - 1
                    }
                }
            }
        }
        i = i + 1
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(three_sum(mutableListOf(0 - 1, 0, 1, 2, 0 - 1, 0 - 4)).toString())
        println(three_sum(mutableListOf(1, 2, 3, 4)).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
