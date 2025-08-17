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

fun unique(nums: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < nums.size) {
        var v: Int = (nums[i]!!).toInt()
        var found: Boolean = false
        var j: Int = (0).toInt()
        while (j < res.size) {
            if (res[j]!! == v) {
                found = true
                break
            }
            j = j + 1
        }
        if (!found) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(v); _tmp }
        }
        i = i + 1
    }
    return res
}

fun array_equalization(vector: MutableList<Int>, step_size: Int): Int {
    if (step_size <= 0) {
        error("Step size must be positive and non-zero.")
    }
    var elems: MutableList<Int> = unique(vector)
    var min_updates: Int = (vector.size).toInt()
    var i: Int = (0).toInt()
    while (i < elems.size) {
        var target: Int = (elems[i]!!).toInt()
        var idx: Int = (0).toInt()
        var updates: Int = (0).toInt()
        while (idx < vector.size) {
            if (vector[idx]!! != target) {
                updates = updates + 1
                idx = idx + step_size
            } else {
                idx = idx + 1
            }
        }
        if (updates < min_updates) {
            min_updates = updates
        }
        i = i + 1
    }
    return min_updates
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(array_equalization(mutableListOf(1, 1, 6, 2, 4, 6, 5, 1, 7, 2, 2, 1, 7, 2, 2), 4)))
        println(_numToStr(array_equalization(mutableListOf(22, 81, 88, 71, 22, 81, 632, 81, 81, 22, 92), 2)))
        println(_numToStr(array_equalization(mutableListOf(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 5)))
        println(_numToStr(array_equalization(mutableListOf(22, 22, 22, 33, 33, 33), 2)))
        println(_numToStr(array_equalization(mutableListOf(1, 2, 3), 2147483647)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
