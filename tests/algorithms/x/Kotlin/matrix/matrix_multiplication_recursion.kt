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

var matrix_1_to_4: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 2), mutableListOf(3, 4))
var matrix_5_to_8: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(5, 6), mutableListOf(7, 8))
var matrix_count_up: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 2, 3, 4), mutableListOf(5, 6, 7, 8), mutableListOf(9, 10, 11, 12), mutableListOf(13, 14, 15, 16))
var matrix_unordered: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(5, 8, 1, 2), mutableListOf(6, 7, 3, 0), mutableListOf(4, 5, 9, 1), mutableListOf(2, 6, 10, 14))
fun is_square(matrix: MutableList<MutableList<Int>>): Boolean {
    var n: Int = (matrix.size).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        if ((matrix[i]!!).size != n) {
            return false
        }
        i = i + 1
    }
    return true
}

fun matrix_multiply(a: MutableList<MutableList<Int>>, b: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var rows: Int = (a.size).toInt()
    var cols: Int = ((b[0]!!).size).toInt()
    var inner: Int = (b.size).toInt()
    var result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < rows) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < cols) {
            var sum: Int = (0).toInt()
            var k: Int = (0).toInt()
            while (k < inner) {
                sum = sum + ((((a[i]!!) as MutableList<Int>))[k]!! * (((b[k]!!) as MutableList<Int>))[j]!!)
                k = k + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(sum); _tmp }
            j = j + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return result
}

fun multiply(i: Int, j: Int, k: Int, a: MutableList<MutableList<Int>>, b: MutableList<MutableList<Int>>, result: MutableList<MutableList<Int>>, n: Int, m: Int): Unit {
    if (i >= n) {
        return
    }
    if (j >= m) {
        multiply(i + 1, 0, 0, a, b, result, n, m)
        return
    }
    if (k >= b.size) {
        multiply(i, j + 1, 0, a, b, result, n, m)
        return
    }
    _listSet(result[i]!!, j, (((result[i]!!) as MutableList<Int>))[j]!! + ((((a[i]!!) as MutableList<Int>))[k]!! * (((b[k]!!) as MutableList<Int>))[j]!!))
    multiply(i, j, k + 1, a, b, result, n, m)
}

fun matrix_multiply_recursive(a: MutableList<MutableList<Int>>, b: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    if ((a.size == 0) || (b.size == 0)) {
        return mutableListOf<MutableList<Int>>()
    }
    if ((((a.size != b.size) || (!is_square(a) as Boolean) as Boolean)) || (!is_square(b) as Boolean)) {
        panic("Invalid matrix dimensions")
    }
    var n: Int = (a.size).toInt()
    var m: Int = ((b[0]!!).size).toInt()
    var result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < m) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            j = j + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    multiply(0, 0, 0, a, b, result, n, m)
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(matrix_multiply_recursive(matrix_1_to_4, matrix_5_to_8))
        println(matrix_multiply_recursive(matrix_count_up, matrix_unordered))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
