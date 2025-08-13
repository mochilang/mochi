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

fun build_set(words: MutableList<String>): MutableMap<String, Boolean> {
    var m: MutableMap<String, Boolean> = mutableMapOf<String, Boolean>()
    for (w in words) {
        (m)[w] = true
    }
    return m
}

fun word_break(s: String, words: MutableList<String>): Boolean {
    var n: Int = (s.length).toInt()
    var dict: MutableMap<String, Boolean> = build_set(words)
    var dp: MutableList<Boolean> = mutableListOf<Boolean>()
    var i: Int = (0).toInt()
    while (i <= n) {
        dp = run { val _tmp = dp.toMutableList(); _tmp.add(false); _tmp }
        i = i + 1
    }
    _listSet(dp, 0, true)
    i = 1
    while (i <= n) {
        var j: Int = (0).toInt()
        while (j < i) {
            if (((dp[j]!!) as Boolean)) {
                var sub: String = _sliceStr(s, j, i)
                if (sub in dict) {
                    _listSet(dp, i, true)
                    j = i
                }
            }
            j = j + 1
        }
        i = i + 1
    }
    return dp[n]!!
}

fun print_bool(b: Boolean): Unit {
    if ((b as Boolean)) {
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
        print_bool(word_break("applepenapple", mutableListOf("apple", "pen")))
        print_bool(word_break("catsandog", mutableListOf("cats", "dog", "sand", "and", "cat")))
        print_bool(word_break("cars", mutableListOf("car", "ca", "rs")))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
