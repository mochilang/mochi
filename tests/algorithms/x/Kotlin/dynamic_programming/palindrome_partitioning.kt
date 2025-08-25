import java.math.BigInteger

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

fun min_partitions(s: String): Int {
    var n: Int = (s.length).toInt()
    var cut: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < n) {
        cut = run { val _tmp = cut.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var pal: MutableList<MutableList<Boolean>> = mutableListOf<MutableList<Boolean>>()
    i = 0
    while (i < n) {
        var row: MutableList<Boolean> = mutableListOf<Boolean>()
        var j: Int = (0).toInt()
        while (j < n) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(false); _tmp }
            j = j + 1
        }
        pal = run { val _tmp = pal.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    i = 0
    while (i < n) {
        var mincut: Int = (i).toInt()
        var j: Int = (0).toInt()
        while (j <= i) {
            if ((s[i].toString() == s[j].toString()) && ((((i - j) < 2) || ((pal[j + 1]!!) as MutableList<Boolean>)[i - 1]!! as Boolean))) {
                _listSet(pal[j]!!, i, true)
                if (j == 0) {
                    mincut = 0
                } else {
                    var candidate: Int = (cut[j - 1]!! + 1).toInt()
                    if (candidate < mincut) {
                        mincut = candidate
                    }
                }
            }
            j = j + 1
        }
        _listSet(cut, i, mincut)
        i = i + 1
    }
    return cut[n - 1]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(min_partitions("aab"))
        println(min_partitions("aaa"))
        println(min_partitions("ababbbabbababa"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
