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

fun min3(a: Int, b: Int, c: Int): Int {
    var m: Int = (a).toInt()
    if (b < m) {
        m = b
    }
    if (c < m) {
        m = c
    }
    return m
}

fun helper_top_down(word1: String, word2: String, dp: MutableList<MutableList<Int>>, i: Int, j: Int): Int {
    if (i < 0) {
        return j + 1
    }
    if (j < 0) {
        return i + 1
    }
    if ((((dp[i]!!) as MutableList<Int>))[j]!! != (0 - 1)) {
        return (((dp[i]!!) as MutableList<Int>))[j]!!
    }
    if (word1.substring(i, i + 1) == word2.substring(j, j + 1)) {
        _listSet(dp[i]!!, j, helper_top_down(word1, word2, dp, i - 1, j - 1))
    } else {
        var insert: Int = (helper_top_down(word1, word2, dp, i, j - 1)).toInt()
        var delete: Int = (helper_top_down(word1, word2, dp, i - 1, j)).toInt()
        var replace: Int = (helper_top_down(word1, word2, dp, i - 1, j - 1)).toInt()
        _listSet(dp[i]!!, j, 1 + min3(insert, delete, replace))
    }
    return (((dp[i]!!) as MutableList<Int>))[j]!!
}

fun min_dist_top_down(word1: String, word2: String): Int {
    var m: Int = (word1.length).toInt()
    var n: Int = (word2.length).toInt()
    var dp: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    for (_u1 in 0 until m) {
        var row: MutableList<Int> = mutableListOf<Int>()
        for (_2 in 0 until n) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0 - 1); _tmp }
        }
        dp = run { val _tmp = dp.toMutableList(); _tmp.add(row); _tmp }
    }
    return helper_top_down(word1, word2, dp, m - 1, n - 1)
}

fun min_dist_bottom_up(word1: String, word2: String): Int {
    var m: Int = (word1.length).toInt()
    var n: Int = (word2.length).toInt()
    var dp: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    for (_u2 in 0 until m + 1) {
        var row: MutableList<Int> = mutableListOf<Int>()
        for (_2 in 0 until n + 1) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
        }
        dp = run { val _tmp = dp.toMutableList(); _tmp.add(row); _tmp }
    }
    for (i in 0 until m + 1) {
        for (j in 0 until n + 1) {
            if (i == 0) {
                _listSet(dp[i]!!, j, j)
            } else {
                if (j == 0) {
                    _listSet(dp[i]!!, j, i)
                } else {
                    if (word1.substring(i - 1, i) == word2.substring(j - 1, j)) {
                        _listSet(dp[i]!!, j, (((dp[i - 1]!!) as MutableList<Int>))[j - 1]!!)
                    } else {
                        var insert: Int = ((((dp[i]!!) as MutableList<Int>))[j - 1]!!).toInt()
                        var delete: Int = ((((dp[i - 1]!!) as MutableList<Int>))[j]!!).toInt()
                        var replace: Int = ((((dp[i - 1]!!) as MutableList<Int>))[j - 1]!!).toInt()
                        _listSet(dp[i]!!, j, 1 + min3(insert, delete, replace))
                    }
                }
            }
        }
    }
    return (((dp[m]!!) as MutableList<Int>))[n]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(min_dist_top_down("intention", "execution").toString())
        println(min_dist_top_down("intention", "").toString())
        println(min_dist_top_down("", "").toString())
        println(min_dist_bottom_up("intention", "execution").toString())
        println(min_dist_bottom_up("intention", "").toString())
        println(min_dist_bottom_up("", "").toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
