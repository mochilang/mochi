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

fun contains_int(xs: MutableList<Int>, x: Int): Boolean {
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (xs[i]!! == x) {
            return true
        }
        i = i + 1
    }
    return false
}

fun contains_string(xs: MutableList<String>, x: String): Boolean {
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (xs[i]!! == x) {
            return true
        }
        i = i + 1
    }
    return false
}

fun count_int(xs: MutableList<Int>, x: Int): Int {
    var cnt: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (xs[i]!! == x) {
            cnt = cnt + 1
        }
        i = i + 1
    }
    return cnt
}

fun count_string(xs: MutableList<String>, x: String): Int {
    var cnt: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (xs[i]!! == x) {
            cnt = cnt + 1
        }
        i = i + 1
    }
    return cnt
}

fun sort_int(xs: MutableList<Int>): MutableList<Int> {
    var arr: MutableList<Int> = xs
    var i: Int = (0).toInt()
    while (i < arr.size) {
        var j: BigInteger = ((i + 1).toBigInteger())
        while (j.compareTo((arr.size).toBigInteger()) < 0) {
            if (arr[(j).toInt()]!! < arr[i]!!) {
                var tmp: Int = (arr[i]!!).toInt()
                _listSet(arr, i, arr[(j).toInt()]!!)
                _listSet(arr, (j).toInt(), tmp)
            }
            j = j.add((1).toBigInteger())
        }
        i = i + 1
    }
    return arr
}

fun sort_string(xs: MutableList<String>): MutableList<String> {
    var arr: MutableList<String> = xs
    var i: Int = (0).toInt()
    while (i < arr.size) {
        var j: BigInteger = ((i + 1).toBigInteger())
        while (j.compareTo((arr.size).toBigInteger()) < 0) {
            if (arr[(j).toInt()]!! < arr[i]!!) {
                var tmp: String = arr[i]!!
                _listSet(arr, i, arr[(j).toInt()]!!)
                _listSet(arr, (j).toInt(), tmp)
            }
            j = j.add((1).toBigInteger())
        }
        i = i + 1
    }
    return arr
}

fun mode_int(lst: MutableList<Int>): MutableList<Int> {
    if (lst.size == 0) {
        return mutableListOf<Int>()
    }
    var counts: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < lst.size) {
        counts = run { val _tmp = counts.toMutableList(); _tmp.add(count_int(lst, lst[i]!!)); _tmp }
        i = i + 1
    }
    var max_count: Int = (0).toInt()
    i = 0
    while (i < counts.size) {
        if (counts[i]!! > max_count) {
            max_count = counts[i]!!
        }
        i = i + 1
    }
    var modes: MutableList<Int> = mutableListOf<Int>()
    i = 0
    while (i < lst.size) {
        if (counts[i]!! == max_count) {
            var v: Int = (lst[i]!!).toInt()
            if (!contains_int(modes, v)) {
                modes = run { val _tmp = modes.toMutableList(); _tmp.add(v); _tmp }
            }
        }
        i = i + 1
    }
    return sort_int(modes)
}

fun mode_string(lst: MutableList<String>): MutableList<String> {
    if (lst.size == 0) {
        return mutableListOf<String>()
    }
    var counts: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < lst.size) {
        counts = run { val _tmp = counts.toMutableList(); _tmp.add(count_string(lst, lst[i]!!)); _tmp }
        i = i + 1
    }
    var max_count: Int = (0).toInt()
    i = 0
    while (i < counts.size) {
        if (counts[i]!! > max_count) {
            max_count = counts[i]!!
        }
        i = i + 1
    }
    var modes: MutableList<String> = mutableListOf<String>()
    i = 0
    while (i < lst.size) {
        if (counts[i]!! == max_count) {
            var v: String = lst[i]!!
            if (!contains_string(modes, v)) {
                modes = run { val _tmp = modes.toMutableList(); _tmp.add(v); _tmp }
            }
        }
        i = i + 1
    }
    return sort_string(modes)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(mode_int(mutableListOf(2, 3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 2, 2, 2)))
        println(mode_int(mutableListOf(3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 4, 2, 2, 2)))
        println(mode_int(mutableListOf(3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 4, 4, 2, 2, 4, 2)))
        println(mode_string(mutableListOf("x", "y", "y", "z")))
        println(mode_string(mutableListOf("x", "x", "y", "y", "z")))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
