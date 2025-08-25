val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/dynamic_programming"

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

fun longest_common_substring(text1: String, text2: String): String {
    if ((text1.length == 0) || (text2.length == 0)) {
        return ""
    }
    var m: Int = (text1.length).toInt()
    var n: Int = (text2.length).toInt()
    var dp: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < (m + 1)) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < (n + 1)) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            j = j + 1
        }
        dp = run { val _tmp = dp.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    var end_pos: Int = (0).toInt()
    var max_len: Int = (0).toInt()
    var ii: Int = (1).toInt()
    while (ii <= m) {
        var jj: Int = (1).toInt()
        while (jj <= n) {
            if (text1.substring(ii - 1, ii) == text2.substring(jj - 1, jj)) {
                _listSet(dp[ii]!!, jj, 1 + ((dp[ii - 1]!!) as MutableList<Int>)[jj - 1]!!)
                if (((dp[ii]!!) as MutableList<Int>)[jj]!! > max_len) {
                    max_len = ((dp[ii]!!) as MutableList<Int>)[jj]!!
                    end_pos = ii
                }
            }
            jj = jj + 1
        }
        ii = ii + 1
    }
    return text1.substring(end_pos - max_len, end_pos)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(longest_common_substring("abcdef", "xabded"))
        println("\n")
        println(longest_common_substring("zxabcdezy", "yzabcdezx"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
