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

fun index_of(s: String, ch: String): Int {
    var i: Int = (0).toInt()
    while (i < s.length) {
        if (s[i].toString() == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun ord(ch: String): Int {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    var idx: Int = (index_of(upper, ch)).toInt()
    if (idx >= 0) {
        return 65 + idx
    }
    idx = index_of(lower, ch)
    if (idx >= 0) {
        return 97 + idx
    }
    return 0
}

fun chr(n: Int): String {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    if ((n >= 65) && (n < 91)) {
        return _sliceStr(upper, n - 65, n - 64)
    }
    if ((n >= 97) && (n < 123)) {
        return _sliceStr(lower, n - 97, n - 96)
    }
    return "?"
}

fun to_upper_char(c: String): String {
    var code: Int = (ord(c)).toInt()
    if ((code >= 97) && (code <= 122)) {
        return chr(code - 32)
    }
    return c
}

fun is_lower(c: String): Boolean {
    var code: Int = (ord(c)).toInt()
    return (((code >= 97) && (code <= 122)) as Boolean)
}

fun abbr(a: String, b: String): Boolean {
    var n: Int = (a.length).toInt()
    var m: Int = (b.length).toInt()
    var dp: MutableList<MutableList<Boolean>> = mutableListOf<MutableList<Boolean>>()
    var i: Int = (0).toInt()
    while (i <= n) {
        var row: MutableList<Boolean> = mutableListOf<Boolean>()
        var j: Int = (0).toInt()
        while (j <= m) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(false); _tmp }
            j = j + 1
        }
        dp = run { val _tmp = dp.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    _listSet(dp[0]!!, 0, true)
    i = 0
    while (i < n) {
        var j: Int = (0).toInt()
        while (j <= m) {
            if ((((((dp[i]!!) as MutableList<Boolean>))[j]!!) as Boolean)) {
                if ((j < m) && (to_upper_char(a[i].toString()) == b[j].toString())) {
                    _listSet(dp[i + 1]!!, j + 1, true)
                }
                if (((is_lower(a[i].toString())) as Boolean)) {
                    _listSet(dp[i + 1]!!, j, true)
                }
            }
            j = j + 1
        }
        i = i + 1
    }
    return (((dp[n]!!) as MutableList<Boolean>))[m]!!
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
        print_bool(abbr("daBcd", "ABC"))
        print_bool(abbr("dBcd", "ABC"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
