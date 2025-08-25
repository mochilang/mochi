val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/dynamic_programming"

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

var m1: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(2, 1), mutableListOf(3, 1), mutableListOf(4, 2))
var m2: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(2, 1, 4), mutableListOf(2, 1, 3), mutableListOf(3, 2, 1))
fun min_int(a: Int, b: Int): Int {
    if (a < b) {
        return a
    }
    return b
}

fun minimum_cost_path(matrix: MutableList<MutableList<Int>>): Int {
    var rows: Int = (matrix.size).toInt()
    var cols: Int = ((matrix[0]!!).size).toInt()
    var j: Int = (1).toInt()
    while (j < cols) {
        var row0: MutableList<Int> = matrix[0]!!
        _listSet(row0, j, row0[j]!! + row0[j - 1]!!)
        _listSet(matrix, 0, row0)
        j = j + 1
    }
    var i: Int = (1).toInt()
    while (i < rows) {
        var row: MutableList<Int> = matrix[i]!!
        _listSet(row, 0, row[0]!! + ((matrix[i - 1]!!) as MutableList<Int>)[0]!!)
        _listSet(matrix, i, row)
        i = i + 1
    }
    i = 1
    while (i < rows) {
        var row: MutableList<Int> = matrix[i]!!
        j = 1
        while (j < cols) {
            var up: Int = (((matrix[i - 1]!!) as MutableList<Int>)[j]!!).toInt()
            var left: Int = (row[j - 1]!!).toInt()
            var best: Int = (min_int(up, left)).toInt()
            _listSet(row, j, row[j]!! + best)
            j = j + 1
        }
        _listSet(matrix, i, row)
        i = i + 1
    }
    return ((matrix[rows - 1]!!) as MutableList<Int>)[cols - 1]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(minimum_cost_path(m1)))
        println(_numToStr(minimum_cost_path(m2)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
