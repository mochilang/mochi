import java.math.BigInteger

fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
}

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

fun reverse(s: String): String {
    var result: String = ""
    var i: Int = (s.length - 1).toInt()
    while (i >= 0) {
        result = result + _sliceStr(s, i, i + 1)
        i = i - 1
    }
    return result
}

fun max_int(a: Int, b: Int): Int {
    if (a > b) {
        return a
    }
    return b
}

fun longest_palindromic_subsequence(s: String): Int {
    var rev: String = reverse(s)
    var n: Int = (s.length).toInt()
    var m: Int = (rev.length).toInt()
    var dp: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i <= n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j <= m) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            j = j + 1
        }
        dp = run { val _tmp = dp.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    i = 1
    while (i <= n) {
        var j: Int = (1).toInt()
        while (j <= m) {
            var a_char: String = _sliceStr(s, i - 1, i)
            var b_char: String = _sliceStr(rev, j - 1, j)
            if (a_char == b_char) {
                _listSet(dp[i]!!, j, 1 + (((dp[i - 1]!!) as MutableList<Int>))[j - 1]!!)
            } else {
                _listSet(dp[i]!!, j, max_int((((dp[i - 1]!!) as MutableList<Int>))[j]!!, (((dp[i]!!) as MutableList<Int>))[j - 1]!!))
            }
            j = j + 1
        }
        i = i + 1
    }
    return (((dp[n]!!) as MutableList<Int>))[m]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(longest_palindromic_subsequence("bbbab").toString())
        println(longest_palindromic_subsequence("bbabcbcab").toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
