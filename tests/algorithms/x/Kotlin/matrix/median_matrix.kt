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

var matrix1: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 3, 5), mutableListOf(2, 6, 9), mutableListOf(3, 6, 9))
fun bubble_sort(a: MutableList<Int>): MutableList<Int> {
    var arr: MutableList<Int> = a
    var n: Int = (arr.size).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        var j: Int = (0).toInt()
        while ((j + 1) < (n - i)) {
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

fun median(matrix: MutableList<MutableList<Int>>): Int {
    var linear: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < matrix.size) {
        var row: MutableList<Int> = matrix[i]!!
        var j: Int = (0).toInt()
        while (j < row.size) {
            linear = run { val _tmp = linear.toMutableList(); _tmp.add(row[j]!!); _tmp }
            j = j + 1
        }
        i = i + 1
    }
    var sorted: MutableList<Int> = bubble_sort(linear)
    var mid: Int = ((sorted.size - 1) / 2).toInt()
    return sorted[mid]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(median(matrix1).toString())
        var matrix2: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 2, 3), mutableListOf(4, 5, 6))
        println(median(matrix2).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
