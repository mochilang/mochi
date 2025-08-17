import java.math.BigInteger

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

fun binary_search(arr: MutableList<Int>, lower_bound: Int, upper_bound: Int, value: Int): Int {
    var r: Int = ((lower_bound + upper_bound) / 2).toInt()
    if (arr[r]!! == value) {
        return r
    }
    if (lower_bound >= upper_bound) {
        return 0 - 1
    }
    if (arr[r]!! < value) {
        return binary_search(arr, r + 1, upper_bound, value)
    }
    return binary_search(arr, lower_bound, r - 1, value)
}

fun mat_bin_search(value: Int, matrix: MutableList<MutableList<Int>>): MutableList<Int> {
    var index: Int = (0).toInt()
    if (((matrix[index]!!) as MutableList<Int>)[0]!! == value) {
        return mutableListOf(index, 0)
    }
    while ((index < matrix.size) && (((matrix[index]!!) as MutableList<Int>)[0]!! < value)) {
        var r: Int = (binary_search(matrix[index]!!, 0, (matrix[index]!!).size - 1, value)).toInt()
        if (r != (0 - 1)) {
            return mutableListOf(index, r)
        }
        index = index + 1
    }
    return mutableListOf(0 - 1, 0 - 1)
}

fun user_main(): Unit {
    var row: MutableList<Int> = mutableListOf(1, 4, 7, 11, 15)
    println(_numToStr(binary_search(row, 0, row.size - 1, 1)))
    println(_numToStr(binary_search(row, 0, row.size - 1, 23)))
    var matrix: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 4, 7, 11, 15), mutableListOf(2, 5, 8, 12, 19), mutableListOf(3, 6, 9, 16, 22), mutableListOf(10, 13, 14, 17, 24), mutableListOf(18, 21, 23, 26, 30))
    println(mat_bin_search(1, matrix).toString())
    println(mat_bin_search(34, matrix).toString())
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
