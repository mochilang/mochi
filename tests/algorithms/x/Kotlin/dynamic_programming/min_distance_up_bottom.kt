import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/dynamic_programming"

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
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

fun helper(word1: String, word2: String, cache: MutableList<MutableList<Int>>, i: Int, j: Int, len1: Int, len2: Int): Int {
    if (i >= len1) {
        return len2 - j
    }
    if (j >= len2) {
        return len1 - i
    }
    if (((cache[i]!!) as MutableList<Int>)[j]!! != (0 - 1)) {
        return ((cache[i]!!) as MutableList<Int>)[j]!!
    }
    var diff: Int = (0).toInt()
    if (word1.substring(i, i + 1) != word2.substring(j, j + 1)) {
        diff = 1
    }
    var delete_cost: Int = (1 + helper(word1, word2, cache, i + 1, j, len1, len2)).toInt()
    var insert_cost: Int = (1 + helper(word1, word2, cache, i, j + 1, len1, len2)).toInt()
    var replace_cost: Int = (diff + helper(word1, word2, cache, i + 1, j + 1, len1, len2)).toInt()
    _listSet(cache[i]!!, j, min3(delete_cost, insert_cost, replace_cost))
    return ((cache[i]!!) as MutableList<Int>)[j]!!
}

fun min_distance_up_bottom(word1: String, word2: String): Int {
    var len1: Int = (word1.length).toInt()
    var len2: Int = (word2.length).toInt()
    var cache: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    for (_u1 in 0 until len1) {
        var row: MutableList<Int> = mutableListOf<Int>()
        for (_2 in 0 until len2) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0 - 1); _tmp }
        }
        cache = run { val _tmp = cache.toMutableList(); _tmp.add(row); _tmp }
    }
    return helper(word1, word2, cache, 0, 0, len1, len2)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(min_distance_up_bottom("intention", "execution")))
        println(_numToStr(min_distance_up_bottom("intention", "")))
        println(_numToStr(min_distance_up_bottom("", "")))
        println(_numToStr(min_distance_up_bottom("zooicoarchaeologist", "zoologist")))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
