import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/linked_list"

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

data class Node(var data: Int = 0, var prev_index: Int = 0, var next_index: Int = 0)
data class LinkedList(var nodes: MutableList<Node> = mutableListOf<Node>(), var head_idx: Int = 0, var tail_idx: Int = 0)
fun empty_list(): LinkedList {
    return LinkedList(nodes = mutableListOf<Node>(), head_idx = 0 - 1, tail_idx = 0 - 1)
}

fun get_head_data(ll: LinkedList): Int {
    if (ll.head_idx == (0 - 1)) {
        return 0 - 1
    }
    var node: Node = (ll.nodes)[ll.head_idx]!!
    return node.data
}

fun get_tail_data(ll: LinkedList): Int {
    if (ll.tail_idx == (0 - 1)) {
        return 0 - 1
    }
    var node: Node = (ll.nodes)[ll.tail_idx]!!
    return node.data
}

fun insert_before_node(ll: LinkedList, idx: Int, new_idx: Int): Unit {
    var nodes: MutableList<Node> = ll.nodes
    var new_node: Node = nodes[new_idx]!!
    new_node.next_index = idx
    var node: Node = nodes[idx]!!
    var p: Int = (node.prev_index).toInt()
    new_node.prev_index = p
    _listSet(nodes, new_idx, new_node)
    if (p == (0 - 1)) {
        ll.head_idx = new_idx
    } else {
        var prev_node: Node = nodes[p]!!
        prev_node.next_index = new_idx
        _listSet(nodes, p, prev_node)
    }
    node.prev_index = new_idx
    _listSet(nodes, idx, node)
    ll.nodes = nodes
}

fun insert_after_node(ll: LinkedList, idx: Int, new_idx: Int): Unit {
    var nodes: MutableList<Node> = ll.nodes
    var new_node: Node = nodes[new_idx]!!
    new_node.prev_index = idx
    var node: Node = nodes[idx]!!
    var nxt: Int = (node.next_index).toInt()
    new_node.next_index = nxt
    _listSet(nodes, new_idx, new_node)
    if (nxt == (0 - 1)) {
        ll.tail_idx = new_idx
    } else {
        var next_node: Node = nodes[nxt]!!
        next_node.prev_index = new_idx
        _listSet(nodes, nxt, next_node)
    }
    node.next_index = new_idx
    _listSet(nodes, idx, node)
    ll.nodes = nodes
}

fun set_head(ll: LinkedList, idx: Int): Unit {
    if (ll.head_idx == (0 - 1)) {
        ll.head_idx = idx
        ll.tail_idx = idx
    } else {
        insert_before_node(ll, ll.head_idx, idx)
    }
}

fun set_tail(ll: LinkedList, idx: Int): Unit {
    if (ll.tail_idx == (0 - 1)) {
        ll.head_idx = idx
        ll.tail_idx = idx
    } else {
        insert_after_node(ll, ll.tail_idx, idx)
    }
}

fun insert(ll: LinkedList, value: Int): Unit {
    var nodes: MutableList<Node> = ll.nodes
    nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(Node(data = value, prev_index = 0 - 1, next_index = 0 - 1)); _tmp }
    var idx: Int = (nodes.size - 1).toInt()
    ll.nodes = nodes
    if (ll.head_idx == (0 - 1)) {
        ll.head_idx = idx
        ll.tail_idx = idx
    } else {
        insert_after_node(ll, ll.tail_idx, idx)
    }
}

fun insert_at_position(ll: LinkedList, position: Int, value: Int): Unit {
    var current: Int = (ll.head_idx).toInt()
    var current_pos: Int = (1).toInt()
    while (current != (0 - 1)) {
        if (current_pos == position) {
            var nodes: MutableList<Node> = ll.nodes
            nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(Node(data = value, prev_index = 0 - 1, next_index = 0 - 1)); _tmp }
            var new_idx: Int = (nodes.size - 1).toInt()
            ll.nodes = nodes
            insert_before_node(ll, current, new_idx)
            return
        }
        var node: Node = (ll.nodes)[current]!!
        current = node.next_index
        current_pos = current_pos + 1
    }
    insert(ll, value)
}

fun get_node(ll: LinkedList, item: Int): Int {
    var current: Int = (ll.head_idx).toInt()
    while (current != (0 - 1)) {
        var node: Node = (ll.nodes)[current]!!
        if (node.data == item) {
            return current
        }
        current = node.next_index
    }
    return 0 - 1
}

fun remove_node_pointers(ll: LinkedList, idx: Int): Unit {
    var nodes: MutableList<Node> = ll.nodes
    var node: Node = nodes[idx]!!
    var nxt: Int = (node.next_index).toInt()
    var p: Int = (node.prev_index).toInt()
    if (nxt != (0 - 1)) {
        var nxt_node: Node = nodes[nxt]!!
        nxt_node.prev_index = p
        _listSet(nodes, nxt, nxt_node)
    }
    if (p != (0 - 1)) {
        var prev_node: Node = nodes[p]!!
        prev_node.next_index = nxt
        _listSet(nodes, p, prev_node)
    }
    node.next_index = 0 - 1
    node.prev_index = 0 - 1
    _listSet(nodes, idx, node)
    ll.nodes = nodes
}

fun delete_value(ll: LinkedList, value: Int): Unit {
    var idx: Int = (get_node(ll, value)).toInt()
    if (idx == (0 - 1)) {
        return
    }
    if (idx == ll.head_idx) {
        var node: Node = (ll.nodes)[idx]!!
        ll.head_idx = node.next_index
    }
    if (idx == ll.tail_idx) {
        var node: Node = (ll.nodes)[idx]!!
        ll.tail_idx = node.prev_index
    }
    remove_node_pointers(ll, idx)
}

fun contains(ll: LinkedList, value: Int): Boolean {
    return get_node(ll, value) != (0 - 1)
}

fun is_empty(ll: LinkedList): Boolean {
    return ll.head_idx == (0 - 1)
}

fun to_string(ll: LinkedList): String {
    var res: String = ""
    var first: Boolean = true
    var current: Int = (ll.head_idx).toInt()
    while (current != (0 - 1)) {
        var node: Node = (ll.nodes)[current]!!
        var _val: String = _numToStr(node.data)
        if (first as Boolean) {
            res = _val
            first = false
        } else {
            res = (res + " ") + _val
        }
        current = node.next_index
    }
    return res
}

fun print_list(ll: LinkedList): Unit {
    var current: Int = (ll.head_idx).toInt()
    while (current != (0 - 1)) {
        var node: Node = (ll.nodes)[current]!!
        println(_numToStr(node.data))
        current = node.next_index
    }
}

fun user_main(): Unit {
    var ll: LinkedList = empty_list()
    println(_numToStr(get_head_data(ll)))
    println(_numToStr(get_tail_data(ll)))
    println(is_empty(ll).toString())
    insert(ll, 10)
    println(_numToStr(get_head_data(ll)))
    println(_numToStr(get_tail_data(ll)))
    insert_at_position(ll, 3, 20)
    println(_numToStr(get_head_data(ll)))
    println(_numToStr(get_tail_data(ll)))
    var nodes: MutableList<Node> = ll.nodes
    nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(Node(data = 1000, prev_index = 0 - 1, next_index = 0 - 1)); _tmp }
    var idx_head: Int = (nodes.size - 1).toInt()
    ll.nodes = nodes
    set_head(ll, idx_head)
    nodes = ll.nodes
    nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(Node(data = 2000, prev_index = 0 - 1, next_index = 0 - 1)); _tmp }
    var idx_tail: Int = (nodes.size - 1).toInt()
    ll.nodes = nodes
    set_tail(ll, idx_tail)
    print_list(ll)
    println(is_empty(ll).toString())
    print_list(ll)
    println(contains(ll, 10).toString())
    delete_value(ll, 10)
    println(contains(ll, 10).toString())
    delete_value(ll, 2000)
    println(_numToStr(get_tail_data(ll)))
    delete_value(ll, 1000)
    println(_numToStr(get_tail_data(ll)))
    println(_numToStr(get_head_data(ll)))
    print_list(ll)
    delete_value(ll, 20)
    print_list(ll)
    var i: Int = (1).toInt()
    while (i < 10) {
        insert(ll, i)
        i = i + 1
    }
    print_list(ll)
    var ll2: LinkedList = empty_list()
    insert_at_position(ll2, 1, 10)
    println(to_string(ll2))
    insert_at_position(ll2, 2, 20)
    println(to_string(ll2))
    insert_at_position(ll2, 1, 30)
    println(to_string(ll2))
    insert_at_position(ll2, 3, 40)
    println(to_string(ll2))
    insert_at_position(ll2, 5, 50)
    println(to_string(ll2))
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
