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

data class Node(var data: Int = 0, var left: Int = 0, var right: Int = 0)
var small: MutableList<Node> = small_tree()
fun inorder(nodes: MutableList<Node>, index: Int, acc: MutableList<Int>): MutableList<Int> {
    if (index == (0 - 1)) {
        return acc
    }
    var node: Node = nodes[index]!!
    var res: MutableList<Int> = inorder(nodes, node.left, acc)
    res = run { val _tmp = res.toMutableList(); _tmp.add(node.data); _tmp }
    res = inorder(nodes, node.right, res)
    return res
}

fun size(nodes: MutableList<Node>, index: Int): Int {
    if (index == (0 - 1)) {
        return 0
    }
    var node: Node = nodes[index]!!
    return (1 + size(nodes, node.left)) + size(nodes, node.right)
}

fun depth(nodes: MutableList<Node>, index: Int): Int {
    if (index == (0 - 1)) {
        return 0
    }
    var node: Node = nodes[index]!!
    var left_depth: Int = (depth(nodes, node.left)).toInt()
    var right_depth: Int = (depth(nodes, node.right)).toInt()
    if (left_depth > right_depth) {
        return left_depth + 1
    }
    return right_depth + 1
}

fun is_full(nodes: MutableList<Node>, index: Int): Boolean {
    if (index == (0 - 1)) {
        return true
    }
    var node: Node = nodes[index]!!
    if ((node.left == (0 - 1)) && (node.right == (0 - 1))) {
        return true
    }
    if ((node.left != (0 - 1)) && (node.right != (0 - 1))) {
        return ((is_full(nodes, node.left) && is_full(nodes, node.right)) as Boolean)
    }
    return false
}

fun small_tree(): MutableList<Node> {
    var arr: MutableList<Node> = mutableListOf<Node>()
    arr = run { val _tmp = arr.toMutableList(); _tmp.add(Node(data = 2, left = 1, right = 2)); _tmp }
    arr = run { val _tmp = arr.toMutableList(); _tmp.add(Node(data = 1, left = 0 - 1, right = 0 - 1)); _tmp }
    arr = run { val _tmp = arr.toMutableList(); _tmp.add(Node(data = 3, left = 0 - 1, right = 0 - 1)); _tmp }
    return arr
}

fun medium_tree(): MutableList<Node> {
    var arr: MutableList<Node> = mutableListOf<Node>()
    arr = run { val _tmp = arr.toMutableList(); _tmp.add(Node(data = 4, left = 1, right = 4)); _tmp }
    arr = run { val _tmp = arr.toMutableList(); _tmp.add(Node(data = 2, left = 2, right = 3)); _tmp }
    arr = run { val _tmp = arr.toMutableList(); _tmp.add(Node(data = 1, left = 0 - 1, right = 0 - 1)); _tmp }
    arr = run { val _tmp = arr.toMutableList(); _tmp.add(Node(data = 3, left = 0 - 1, right = 0 - 1)); _tmp }
    arr = run { val _tmp = arr.toMutableList(); _tmp.add(Node(data = 5, left = 0 - 1, right = 5)); _tmp }
    arr = run { val _tmp = arr.toMutableList(); _tmp.add(Node(data = 6, left = 0 - 1, right = 6)); _tmp }
    arr = run { val _tmp = arr.toMutableList(); _tmp.add(Node(data = 7, left = 0 - 1, right = 0 - 1)); _tmp }
    return arr
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(size(small, 0))
        println(inorder(small, 0, mutableListOf<Int>()))
        println(depth(small, 0))
        println(is_full(small, 0))
        var medium: MutableList<Node> = medium_tree()
        println(size(medium, 0))
        println(inorder(medium, 0, mutableListOf<Int>()))
        println(depth(medium, 0))
        println(is_full(medium, 0))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
