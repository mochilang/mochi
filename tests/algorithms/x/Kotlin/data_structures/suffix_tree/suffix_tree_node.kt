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

data class SuffixTreeNode(var children: MutableMap<String, Int> = mutableMapOf<String, Int>(), var is_end_of_string: Boolean = false, var start: Int = 0, var end: Int = 0, var suffix_link: Int = 0)
var root: SuffixTreeNode = new_suffix_tree_node(mutableMapOf<String, Int>("a" to (1)), false, 0 - 1, 0 - 1, 0 - 1)
var leaf: SuffixTreeNode = new_suffix_tree_node(mutableMapOf<String, Int>(), true, 0, 2, 0)
var nodes: MutableList<SuffixTreeNode> = mutableListOf(root, leaf)
var root_check: SuffixTreeNode = nodes[0]!!
var leaf_check: SuffixTreeNode = nodes[1]!!
fun new_suffix_tree_node(children: MutableMap<String, Int>, is_end_of_string: Boolean, start: Int, end: Int, suffix_link: Int): SuffixTreeNode {
    return SuffixTreeNode(children = children, is_end_of_string = is_end_of_string, start = start, end = end, suffix_link = suffix_link)
}

fun empty_suffix_tree_node(): SuffixTreeNode {
    return new_suffix_tree_node(mutableMapOf<String, Int>(), false, 0 - 1, 0 - 1, 0 - 1)
}

fun has_key(m: MutableMap<String, Int>, k: String): Boolean {
    for (key in m.keys) {
        if (key == k) {
            return true
        }
    }
    return false
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(has_key(root_check.children, "a").toString())
        println(leaf_check.is_end_of_string.toString())
        println(leaf_check.start.toString())
        println(leaf_check.end.toString())
        println(leaf_check.suffix_link.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
