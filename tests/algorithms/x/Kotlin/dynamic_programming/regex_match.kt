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

fun recursive_match(text: String, pattern: String): Boolean {
    if (pattern.length == 0) {
        return text.length == 0
    }
    if (text.length == 0) {
        if ((pattern.length >= 2) && (pattern.substring(pattern.length - 1, pattern.length) == "*")) {
            return recursive_match(text, pattern.substring(0, pattern.length - 2))
        }
        return false
    }
    var last_text: String = text.substring(text.length - 1, text.length)
    var last_pattern: String = pattern.substring(pattern.length - 1, pattern.length)
    if ((last_text == last_pattern) || (last_pattern == ".")) {
        return recursive_match(text.substring(0, text.length - 1), pattern.substring(0, pattern.length - 1))
    }
    if (last_pattern == "*") {
        if ((recursive_match(text.substring(0, text.length - 1), pattern)) as Boolean) {
            return true
        }
        return recursive_match(text, pattern.substring(0, pattern.length - 2))
    }
    return false
}

fun dp_match(text: String, pattern: String): Boolean {
    var m: Int = (text.length).toInt()
    var n: Int = (pattern.length).toInt()
    var dp: MutableList<MutableList<Boolean>> = mutableListOf<MutableList<Boolean>>()
    var i: Int = (0).toInt()
    while (i <= m) {
        var row: MutableList<Boolean> = mutableListOf<Boolean>()
        var j: Int = (0).toInt()
        while (j <= n) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(false); _tmp }
            j = j + 1
        }
        dp = run { val _tmp = dp.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    _listSet(dp[0]!!, 0, true)
    var j: Int = (1).toInt()
    while (j <= n) {
        if ((pattern.substring(j - 1, j) == "*") && (j >= 2)) {
            if ((((dp[0]!!) as MutableList<Boolean>)[j - 2]!!) as Boolean) {
                _listSet(dp[0]!!, j, true)
            }
        }
        j = j + 1
    }
    i = 1
    while (i <= m) {
        j = 1
        while (j <= n) {
            var p_char: String = pattern.substring(j - 1, j)
            var t_char: String = text.substring(i - 1, i)
            if ((p_char == ".") || (p_char == t_char)) {
                if ((((dp[i - 1]!!) as MutableList<Boolean>)[j - 1]!!) as Boolean) {
                    _listSet(dp[i]!!, j, true)
                }
            } else {
                if (p_char == "*") {
                    if (j >= 2) {
                        if ((((dp[i]!!) as MutableList<Boolean>)[j - 2]!!) as Boolean) {
                            _listSet(dp[i]!!, j, true)
                        }
                        var prev_p: String = pattern.substring(j - 2, j - 1)
                        if ((prev_p == ".") || (prev_p == t_char)) {
                            if ((((dp[i - 1]!!) as MutableList<Boolean>)[j]!!) as Boolean) {
                                _listSet(dp[i]!!, j, true)
                            }
                        }
                    }
                } else {
                    _listSet(dp[i]!!, j, false)
                }
            }
            j = j + 1
        }
        i = i + 1
    }
    return ((dp[m]!!) as MutableList<Boolean>)[n]!!
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
        print_bool(recursive_match("abc", "a.c"))
        print_bool(recursive_match("abc", "af*.c"))
        print_bool(recursive_match("abc", "a.c*"))
        print_bool(recursive_match("abc", "a.c*d"))
        print_bool(recursive_match("aa", ".*"))
        print_bool(dp_match("abc", "a.c"))
        print_bool(dp_match("abc", "af*.c"))
        print_bool(dp_match("abc", "a.c*"))
        print_bool(dp_match("abc", "a.c*d"))
        print_bool(dp_match("aa", ".*"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
