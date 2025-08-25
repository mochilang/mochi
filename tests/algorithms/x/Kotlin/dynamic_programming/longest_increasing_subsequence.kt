val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/dynamic_programming"

fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
}

fun _len(v: Any?): Int = when (v) {
    is String -> v.length
    is Collection<*> -> v.size
    is Map<*, *> -> v.size
    else -> v.toString().length
}

fun <T> concat(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    res.addAll(a)
    res.addAll(b)
    return res
}

fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }

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

fun longest_subsequence(xs: MutableList<Int>): MutableList<Int> {
    var n: Int = (xs.size).toInt()
    if (n <= 1) {
        return xs
    }
    var pivot: Int = (xs[0]!!).toInt()
    var is_found: Boolean = false
    var i: Int = (1).toInt()
    var longest_subseq: MutableList<Int> = mutableListOf<Int>()
    while ((!is_found as Boolean) && (i < n)) {
        if (xs[i]!! < pivot) {
            is_found = true
            var temp_array: MutableList<Int> = _sliceList(xs, i, n)
            temp_array = longest_subsequence(temp_array)
            if (_len(temp_array) > longest_subseq.size) {
                longest_subseq = temp_array as MutableList<Int>
            }
        } else {
            i = i + 1
        }
    }
    var filtered: MutableList<Int> = mutableListOf<Int>()
    var j: Int = (1).toInt()
    while (j < n) {
        if (xs[j]!! >= pivot) {
            filtered = run { val _tmp = filtered.toMutableList(); _tmp.add(xs[j]!!); _tmp }
        }
        j = j + 1
    }
    var candidate: MutableList<Int> = mutableListOf<Int>()
    candidate = run { val _tmp = candidate.toMutableList(); _tmp.add(pivot); _tmp }
    candidate = concat(candidate, longest_subsequence(filtered))
    if (candidate.size > longest_subseq.size) {
        return candidate
    } else {
        return longest_subseq
    }
}

fun test_examples(): Unit {
    expect(longest_subsequence(mutableListOf(10, 22, 9, 33, 21, 50, 41, 60, 80)) == mutableListOf(10, 22, 33, 41, 60, 80))
    expect(longest_subsequence(mutableListOf(4, 8, 7, 5, 1, 12, 2, 3, 9)) == mutableListOf(1, 2, 3, 9))
    expect(longest_subsequence(mutableListOf(28, 26, 12, 23, 35, 39)) == mutableListOf(12, 23, 35, 39))
    expect(longest_subsequence(mutableListOf(9, 8, 7, 6, 5, 7)) == mutableListOf(5, 7))
    expect(longest_subsequence(mutableListOf(1, 1, 1)) == mutableListOf(1, 1, 1))
    expect(longest_subsequence(mutableListOf<Int>()) == mutableListOf<Any?>())
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        test_examples()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
