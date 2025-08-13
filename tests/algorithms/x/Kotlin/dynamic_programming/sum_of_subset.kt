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

fun create_bool_matrix(rows: Int, cols: Int): MutableList<MutableList<Boolean>> {
    var matrix: MutableList<MutableList<Boolean>> = mutableListOf<MutableList<Boolean>>()
    var i: Int = (0).toInt()
    while (i <= rows) {
        var row: MutableList<Boolean> = mutableListOf<Boolean>()
        var j: Int = (0).toInt()
        while (j <= cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(false); _tmp }
            j = j + 1
        }
        matrix = run { val _tmp = matrix.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return matrix
}

fun is_sum_subset(arr: MutableList<Int>, required_sum: Int): Boolean {
    var arr_len: Int = (arr.size).toInt()
    var subset: MutableList<MutableList<Boolean>> = create_bool_matrix(arr_len, required_sum)
    var i: Int = (0).toInt()
    while (i <= arr_len) {
        _listSet(subset[i]!!, 0, true)
        i = i + 1
    }
    var j: Int = (1).toInt()
    while (j <= required_sum) {
        _listSet(subset[0]!!, j, false)
        j = j + 1
    }
    i = 1
    while (i <= arr_len) {
        j = 1
        while (j <= required_sum) {
            if (arr[i - 1]!! > j) {
                _listSet(subset[i]!!, j, (((subset[i - 1]!!) as MutableList<Boolean>))[j]!!)
            }
            if (arr[i - 1]!! <= j) {
                _listSet(subset[i]!!, j, (((((subset[i - 1]!!) as MutableList<Boolean>))[j]!! || (((subset[i - 1]!!) as MutableList<Boolean>))[j - arr[i - 1]!!]!!) as Boolean))
            }
            j = j + 1
        }
        i = i + 1
    }
    return (((subset[arr_len]!!) as MutableList<Boolean>))[required_sum]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(is_sum_subset(mutableListOf(2, 4, 6, 8), 5))
        println(is_sum_subset(mutableListOf(2, 4, 6, 8), 14))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
