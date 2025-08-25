val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/dynamic_programming"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

fun make_list(len: Int, value: Int): MutableList<Int> {
    var arr: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < len) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(value); _tmp }
        i = i + 1
    }
    return arr
}

fun trapped_rainwater(heights: MutableList<Int>): Int {
    if (heights.size == 0) {
        return 0
    }
    var i: Int = (0).toInt()
    while (i < heights.size) {
        if (heights[i]!! < 0) {
            panic("No height can be negative")
        }
        i = i + 1
    }
    var length: Int = (heights.size).toInt()
    var left_max: MutableList<Int> = make_list(length, 0)
    _listSet(left_max, 0, heights[0]!!)
    i = 1
    while (i < length) {
        if (heights[i]!! > left_max[i - 1]!!) {
            _listSet(left_max, i, heights[i]!!)
        } else {
            _listSet(left_max, i, left_max[i - 1]!!)
        }
        i = i + 1
    }
    var right_max: MutableList<Int> = make_list(length, 0)
    var last: Int = (length - 1).toInt()
    _listSet(right_max, last, heights[last]!!)
    i = last - 1
    while (i >= 0) {
        if (heights[i]!! > right_max[i + 1]!!) {
            _listSet(right_max, i, heights[i]!!)
        } else {
            _listSet(right_max, i, right_max[i + 1]!!)
        }
        i = i - 1
    }
    var total: Int = (0).toInt()
    i = 0
    while (i < length) {
        var left: Int = (left_max[i]!!).toInt()
        var right: Int = (right_max[i]!!).toInt()
        var smaller: Int = (if (left < right) left else right).toInt()
        total = total + (smaller - heights[i]!!)
        i = i + 1
    }
    return total
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(trapped_rainwater(mutableListOf(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1))))
        println(_numToStr(trapped_rainwater(mutableListOf(7, 1, 5, 3, 6, 4))))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
