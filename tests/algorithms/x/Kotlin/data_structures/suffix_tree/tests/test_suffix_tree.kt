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

data class SuffixTree(var text: String = "")
var text: String = "banana"
var st: SuffixTree = suffix_tree_new(text)
var patterns_exist: MutableList<String> = mutableListOf("ana", "ban", "na")
var i: Int = 0
var patterns_none: MutableList<String> = mutableListOf("xyz", "apple", "cat")
fun suffix_tree_new(text: String): SuffixTree {
    return SuffixTree(text = text)
}

fun suffix_tree_search(st: SuffixTree, pattern: String): Boolean {
    if (pattern.length == 0) {
        return true
    }
    var i: Int = 0
    var n: Int = (st.text).length
    var m: Int = pattern.length
    while (i <= (n - m)) {
        var j: Int = 0
        var found: Boolean = true
        while (j < m) {
            if ((st.text)[i + j].toString() != pattern[j].toString()) {
                found = false
                break
            }
            j = j + 1
        }
        if ((found as Boolean)) {
            return true
        }
        i = i + 1
    }
    return false
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (i < patterns_exist.size) {
            println(suffix_tree_search(st, patterns_exist[i]!!).toString())
            i = i + 1
        }
        i = 0
        while (i < patterns_none.size) {
            println(suffix_tree_search(st, patterns_none[i]!!).toString())
            i = i + 1
        }
        println(suffix_tree_search(st, "").toString())
        println(suffix_tree_search(st, text).toString())
        var substrings: MutableList<String> = mutableListOf("ban", "ana", "a", "na")
        i = 0
        while (i < substrings.size) {
            println(suffix_tree_search(st, substrings[i]!!).toString())
            i = i + 1
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
