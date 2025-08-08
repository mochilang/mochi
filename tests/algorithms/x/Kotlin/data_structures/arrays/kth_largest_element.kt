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

var arr1: MutableList<Int> = mutableListOf(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
fun partition(arr: MutableList<Int>, low: Int, high: Int): Int {
    var pivot: Int = (arr[high]!!).toInt()
    var i: BigInteger = ((low - 1).toBigInteger())
    var j: Int = (low).toInt()
    while (j < high) {
        if (arr[j]!! >= pivot) {
            i = i.add((1).toBigInteger())
            var tmp: Int = (arr[(i).toInt()]!!).toInt()
            _listSet(arr, (i).toInt(), arr[j]!!)
            _listSet(arr, j, tmp)
        }
        j = j + 1
    }
    var k: Int = (i.add((1).toBigInteger())).toInt()
    var tmp: Int = (arr[k]!!).toInt()
    _listSet(arr, k, arr[high]!!)
    _listSet(arr, high, tmp)
    return k
}

fun kth_largest_element(arr: MutableList<Int>, position: Int): Int {
    if (arr.size == 0) {
        return 0 - 1
    }
    if ((position < 1) || (position > arr.size)) {
        return 0 - 1
    }
    var low: Int = (0).toInt()
    var high: BigInteger = ((arr.size - 1).toBigInteger())
    while ((low).toBigInteger().compareTo((high)) <= 0) {
        if ((low > (arr.size - 1)) || (high.compareTo((0).toBigInteger()) < 0)) {
            return 0 - 1
        }
        var pivot_index: Int = (partition(arr, low, (high.toInt()))).toInt()
        if (pivot_index == (position - 1)) {
            return arr[pivot_index]!!
        } else {
            if (pivot_index > (position - 1)) {
                high = ((pivot_index - 1).toBigInteger())
            } else {
                low = pivot_index + 1
            }
        }
    }
    return 0 - 1
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(kth_largest_element(arr1, 3))
        println("\n")
        var arr2: MutableList<Int> = mutableListOf(2, 5, 6, 1, 9, 3, 8, 4, 7, 3, 5)
        println(kth_largest_element(arr2, 1))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
