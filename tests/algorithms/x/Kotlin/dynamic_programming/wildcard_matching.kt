val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/dynamic_programming"

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
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

fun make_bool_list(n: Int): MutableList<Boolean> {
    var row: MutableList<Boolean> = mutableListOf<Boolean>()
    var i: Int = (0).toInt()
    while (i < n) {
        row = run { val _tmp = row.toMutableList(); _tmp.add(false); _tmp }
        i = i + 1
    }
    return row
}

fun make_bool_matrix(rows: Int, cols: Int): MutableList<MutableList<Boolean>> {
    var matrix: MutableList<MutableList<Boolean>> = mutableListOf<MutableList<Boolean>>()
    var i: Int = (0).toInt()
    while (i < rows) {
        matrix = run { val _tmp = matrix.toMutableList(); _tmp.add(make_bool_list(cols)); _tmp }
        i = i + 1
    }
    return matrix
}

fun is_match(s: String, p: String): Boolean {
    var n: Int = (s.length).toInt()
    var m: Int = (p.length).toInt()
    var dp: MutableList<MutableList<Boolean>> = make_bool_matrix(n + 1, m + 1)
    _listSet(dp[0]!!, 0, true)
    var j: Int = (1).toInt()
    while (j <= m) {
        if (_sliceStr(p, j - 1, j) == "*") {
            _listSet(dp[0]!!, j, ((dp[0]!!) as MutableList<Boolean>)[j - 1]!!)
        }
        j = j + 1
    }
    var i: Int = (1).toInt()
    while (i <= n) {
        var j2: Int = (1).toInt()
        while (j2 <= m) {
            var pc: String = _sliceStr(p, j2 - 1, j2)
            var sc: String = _sliceStr(s, i - 1, i)
            if ((pc == sc) || (pc == "?")) {
                _listSet(dp[i]!!, j2, ((dp[i - 1]!!) as MutableList<Boolean>)[j2 - 1]!!)
            } else {
                if (pc == "*") {
                    if (((dp[i - 1]!!) as MutableList<Boolean>)[j2]!! || ((dp[i]!!) as MutableList<Boolean>)[j2 - 1]!!) {
                        _listSet(dp[i]!!, j2, true)
                    }
                }
            }
            j2 = j2 + 1
        }
        i = i + 1
    }
    return ((dp[n]!!) as MutableList<Boolean>)[m]!!
}

fun print_bool(b: Boolean): Unit {
    if (b as Boolean) {
        println(true)
    } else {
        println(false)
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        print_bool(is_match("abc", "a*c"))
        print_bool(is_match("abc", "a*d"))
        print_bool(is_match("baaabab", "*****ba*****ab"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
