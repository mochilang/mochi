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

fun dp_count(s: MutableList<Int>, n: Int): Int {
    if (n < 0) {
        return 0
    }
    var table: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i <= n) {
        table = run { val _tmp = table.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    _listSet(table, 0, 1)
    var idx: Int = (0).toInt()
    while (idx < s.size) {
        var coin_val: Int = (s[idx]!!).toInt()
        var j: Int = (coin_val).toInt()
        while (j <= n) {
            _listSet(table, j, table[j]!! + table[j - coin_val]!!)
            j = j + 1
        }
        idx = idx + 1
    }
    return table[n]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(dp_count(mutableListOf(1, 2, 3), 4))
        println(dp_count(mutableListOf(1, 2, 3), 7))
        println(dp_count(mutableListOf(2, 5, 3, 6), 10))
        println(dp_count(mutableListOf(10), 99))
        println(dp_count(mutableListOf(4, 5, 6), 0))
        println(dp_count(mutableListOf(1, 2, 3), 0 - 5))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
