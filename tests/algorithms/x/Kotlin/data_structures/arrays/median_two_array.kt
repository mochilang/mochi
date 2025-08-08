fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

fun sortFloats(xs: MutableList<Double>): MutableList<Double> {
    var arr: MutableList<Double> = xs
    var i: Int = (0).toInt()
    while (i < arr.size) {
        var j: Int = (0).toInt()
        while (j < (arr.size - 1)) {
            if (arr[j]!! > arr[j + 1]!!) {
                var t: Double = arr[j]!!
                _listSet(arr, j, arr[j + 1]!!)
                _listSet(arr, j + 1, t)
            }
            j = j + 1
        }
        i = i + 1
    }
    return arr
}

fun find_median_sorted_arrays(nums1: MutableList<Double>, nums2: MutableList<Double>): Double {
    if ((nums1.size == 0) && (nums2.size == 0)) {
        panic("Both input arrays are empty.")
    }
    var merged: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < nums1.size) {
        merged = run { val _tmp = merged.toMutableList(); _tmp.add(nums1[i]!!); _tmp }
        i = i + 1
    }
    var j: Int = (0).toInt()
    while (j < nums2.size) {
        merged = run { val _tmp = merged.toMutableList(); _tmp.add(nums2[j]!!); _tmp }
        j = j + 1
    }
    var sorted: MutableList<Double> = sortFloats(merged)
    var total: Int = (sorted.size).toInt()
    if ((Math.floorMod(total, 2)) == 1) {
        return sorted[total / 2]!!
    }
    var middle1: Double = sorted[(total / 2) - 1]!!
    var middle2: Double = sorted[total / 2]!!
    return (middle1 + middle2) / 2.0
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(find_median_sorted_arrays(mutableListOf(1.0, 3.0), mutableListOf(2.0)))
        println(find_median_sorted_arrays(mutableListOf(1.0, 2.0), mutableListOf(3.0, 4.0)))
        println(find_median_sorted_arrays(mutableListOf(0.0, 0.0), mutableListOf(0.0, 0.0)))
        println(find_median_sorted_arrays(mutableListOf<Double>(), mutableListOf(1.0)))
        println(find_median_sorted_arrays(mutableListOf(0.0 - 1000.0), mutableListOf(1000.0)))
        println(find_median_sorted_arrays(mutableListOf(0.0 - 1.1, 0.0 - 2.2), mutableListOf(0.0 - 3.3, 0.0 - 4.4)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
