import java.math.BigInteger

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

data class Node(var children: MutableMap<String, Int> = mutableMapOf<String, Int>(), var is_end_of_string: Boolean = false, var start: Int = 0, var end: Int = 0)
data class SuffixTree(var text: String = "", var nodes: MutableList<Node> = mutableListOf<Node>())
var st: SuffixTree = new_suffix_tree("bananas")
fun new_node(): Node {
    return Node(children = mutableMapOf<String, Int>(), is_end_of_string = false, start = 0 - 1, end = 0 - 1)
}

fun has_key(m: MutableMap<String, Int>, k: String): Boolean {
    for (key in m.keys) {
        if (key == k) {
            return true
        }
    }
    return false
}

fun add_suffix(tree: SuffixTree, suffix: String, index: Int): SuffixTree {
    var nodes: MutableList<Node> = tree.nodes
    var node_idx: Int = 0
    var j: Int = 0
    while (j < suffix.length) {
        var ch: String = suffix.substring(j, j + 1)
        var node: Node = nodes[node_idx]!!
        var children: MutableMap<String, Int> = node.children
        if (!has_key(children, ch)) {
            nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(new_node()); _tmp }
            var new_idx: Int = nodes.size - 1
            (children)[ch] = new_idx
        }
        node.children = children
        _listSet(nodes, node_idx, node)
        node_idx = (children)[ch] as Int
        j = j + 1
    }
    var node: Node = nodes[node_idx]!!
    node.is_end_of_string = true
    node.start = index
    node.end = (index + suffix.length) - 1
    _listSet(nodes, node_idx, node)
    tree.nodes = nodes
    return tree
}

fun build_suffix_tree(tree: SuffixTree): SuffixTree {
    var text: String = tree.text
    var n: Int = text.length
    var i: Int = 0
    var t: SuffixTree = tree
    while (i < n) {
        var suffix: String = ""
        var k: Int = i
        while (k < n) {
            suffix = suffix + text.substring(k, k + 1)
            k = k + 1
        }
        t = add_suffix(t, suffix, i)
        i = i + 1
    }
    return t
}

fun new_suffix_tree(text: String): SuffixTree {
    var tree: SuffixTree = SuffixTree(text = text, nodes = mutableListOf<Node>())
    tree.nodes = run { val _tmp = (tree.nodes).toMutableList(); _tmp.add(new_node()); _tmp }
    tree = build_suffix_tree(tree)
    return tree
}

fun search(tree: SuffixTree, pattern: String): Boolean {
    var node_idx: Int = 0
    var i: Int = 0
    var nodes: MutableList<Node> = tree.nodes
    while (i < pattern.length) {
        var ch: String = pattern.substring(i, i + 1)
        var node: Node = nodes[node_idx]!!
        var children: MutableMap<String, Int> = node.children
        if (!has_key(children, ch)) {
            return false
        }
        node_idx = (children)[ch] as Int
        i = i + 1
    }
    return true
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(search(st, "ana").toString())
        println(search(st, "apple").toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
