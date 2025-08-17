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

fun is_valid_matrix(matrix: MutableList<MutableList<Int>>): Boolean {
    if (matrix.size == 0) {
        return false
    }
    var cols: Int = ((matrix[0]!!).size).toInt()
    for (row in matrix) {
        if (row.size != cols) {
            return false
        }
    }
    return true
}

fun spiral_traversal(matrix: MutableList<MutableList<Int>>): MutableList<Int> {
    if (!is_valid_matrix(matrix)) {
        return mutableListOf<Int>()
    }
    var rows: Int = (matrix.size).toInt()
    var cols: Int = ((matrix[0]!!).size).toInt()
    var top: Int = (0).toInt()
    var bottom: Int = (rows - 1).toInt()
    var left: Int = (0).toInt()
    var right: Int = (cols - 1).toInt()
    var result: MutableList<Int> = mutableListOf<Int>()
    while ((left <= right) && (top <= bottom)) {
        var i: Int = (left).toInt()
        while (i <= right) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(((matrix[top]!!) as MutableList<Int>)[i]!!); _tmp }
            i = i + 1
        }
        top = top + 1
        i = top
        while (i <= bottom) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(((matrix[i]!!) as MutableList<Int>)[right]!!); _tmp }
            i = i + 1
        }
        right = right - 1
        if (top <= bottom) {
            i = right
            while (i >= left) {
                result = run { val _tmp = result.toMutableList(); _tmp.add(((matrix[bottom]!!) as MutableList<Int>)[i]!!); _tmp }
                i = i - 1
            }
            bottom = bottom - 1
        }
        if (left <= right) {
            i = bottom
            while (i >= top) {
                result = run { val _tmp = result.toMutableList(); _tmp.add(((matrix[i]!!) as MutableList<Int>)[left]!!); _tmp }
                i = i - 1
            }
            left = left + 1
        }
    }
    return result
}

fun spiral_print_clockwise(matrix: MutableList<MutableList<Int>>): Unit {
    for (value in spiral_traversal(matrix)) {
        println(_numToStr(value))
    }
}

fun user_main(): Unit {
    var a: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 2, 3, 4), mutableListOf(5, 6, 7, 8), mutableListOf(9, 10, 11, 12))
    spiral_print_clockwise(a)
    println(spiral_traversal(a).toString())
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
