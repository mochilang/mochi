val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/linked_list"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

data class Node(var data: String = "", var prev: Int = 0, var next: Int = 0)
data class LinkedDeque(var nodes: MutableList<Node> = mutableListOf<Node>(), var header: Int = 0, var trailer: Int = 0, var size: Int = 0)
data class DeleteResult(var deque: LinkedDeque = LinkedDeque(nodes = mutableListOf<Node>(), header = 0, trailer = 0, size = 0), var value: String = "")
fun new_deque(): LinkedDeque {
    var nodes: MutableList<Node> = mutableListOf<Node>()
    nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(Node(data = "", prev = 0 - 1, next = 1)); _tmp }
    nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(Node(data = "", prev = 0, next = 0 - 1)); _tmp }
    return LinkedDeque(nodes = nodes, header = 0, trailer = 1, size = 0)
}

fun is_empty(d: LinkedDeque): Boolean {
    return d.size == 0
}

fun front(d: LinkedDeque): String {
    if ((is_empty(d)) as Boolean) {
        panic("List is empty")
    }
    var head: Node = (d.nodes)[d.header]!!
    var idx: Int = (head.next).toInt()
    var node: Node = (d.nodes)[idx]!!
    return node.data
}

fun back(d: LinkedDeque): String {
    if ((is_empty(d)) as Boolean) {
        panic("List is empty")
    }
    var tail: Node = (d.nodes)[d.trailer]!!
    var idx: Int = (tail.prev).toInt()
    var node: Node = (d.nodes)[idx]!!
    return node.data
}

fun insert(d: LinkedDeque, pred: Int, value: String, succ: Int): LinkedDeque {
    var nodes: MutableList<Node> = d.nodes
    var new_idx: Int = (nodes.size).toInt()
    nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(Node(data = value, prev = pred, next = succ)); _tmp }
    var pred_node: Node = nodes[pred]!!
    pred_node.next = new_idx
    _listSet(nodes, pred, pred_node)
    var succ_node: Node = nodes[succ]!!
    succ_node.prev = new_idx
    _listSet(nodes, succ, succ_node)
    d.nodes = nodes
    d.size = d.size + 1
    return d
}

fun delete(d: LinkedDeque, idx: Int): DeleteResult {
    var nodes: MutableList<Node> = d.nodes
    var node: Node = nodes[idx]!!
    var pred: Int = (node.prev).toInt()
    var succ: Int = (node.next).toInt()
    var pred_node: Node = nodes[pred]!!
    pred_node.next = succ
    _listSet(nodes, pred, pred_node)
    var succ_node: Node = nodes[succ]!!
    succ_node.prev = pred
    _listSet(nodes, succ, succ_node)
    var _val: String = node.data
    d.nodes = nodes
    d.size = d.size - 1
    return DeleteResult(deque = d, value = _val)
}

fun add_first(d: LinkedDeque, value: String): LinkedDeque {
    var head: Node = (d.nodes)[d.header]!!
    var succ: Int = (head.next).toInt()
    return insert(d, d.header, value, succ)
}

fun add_last(d: LinkedDeque, value: String): LinkedDeque {
    var tail: Node = (d.nodes)[d.trailer]!!
    var pred: Int = (tail.prev).toInt()
    return insert(d, pred, value, d.trailer)
}

fun remove_first(d: LinkedDeque): DeleteResult {
    if ((is_empty(d)) as Boolean) {
        panic("remove_first from empty list")
    }
    var head: Node = (d.nodes)[d.header]!!
    var idx: Int = (head.next).toInt()
    return delete(d, idx)
}

fun remove_last(d: LinkedDeque): DeleteResult {
    if ((is_empty(d)) as Boolean) {
        panic("remove_first from empty list")
    }
    var tail: Node = (d.nodes)[d.trailer]!!
    var idx: Int = (tail.prev).toInt()
    return delete(d, idx)
}

fun user_main(): Unit {
    var d: LinkedDeque = new_deque()
    d = add_first(d, "A")
    println(front(d))
    d = add_last(d, "B")
    println(back(d))
    var r: DeleteResult = remove_first(d)
    d = r.deque
    println(r.value)
    r = remove_last(d)
    d = r.deque
    println(r.value)
    println(is_empty(d).toString())
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
