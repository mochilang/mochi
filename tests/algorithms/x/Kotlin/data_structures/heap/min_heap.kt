import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/heap"

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

data class Node(var name: String = "", var _val: Int = 0)
data class MinHeap(var heap: MutableList<Node> = mutableListOf<Node>(), var idx_of_element: MutableMap<String, Int> = mutableMapOf<String, Int>(), var heap_dict: MutableMap<String, Int> = mutableMapOf<String, Int>())
var r: Node = Node(name = "R", _val = 0 - 1)
var b: Node = Node(name = "B", _val = 6)
var a: Node = Node(name = "A", _val = 3)
var x: Node = Node(name = "X", _val = 1)
var e: Node = Node(name = "E", _val = 4)
var my_min_heap: MinHeap = new_min_heap(mutableListOf(r, b, a, x, e))
fun get_parent_idx(idx: Int): Int {
    return Math.floorDiv((idx - 1), 2)
}

fun get_left_child_idx(idx: Int): Int {
    return (idx * 2) + 1
}

fun get_right_child_idx(idx: Int): Int {
    return (idx * 2) + 2
}

fun remove_key(m: MutableMap<String, Int>, k: String): MutableMap<String, Int> {
    var out: MutableMap<String, Int> = mutableMapOf<String, Int>()
    for (key in m.keys) {
        if (key != k) {
            (out)[key] = (m)[key] as Int
        }
    }
    return out
}

fun slice_without_last(xs: MutableList<Node>): MutableList<Node> {
    var res: MutableList<Node> = mutableListOf<Node>()
    var i: Int = (0).toInt()
    while (i < (xs.size - 1)) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun sift_down(mh: MinHeap, idx: Int): Unit {
    var heap: MutableList<Node> = mh.heap
    var idx_map: MutableMap<String, Int> = mh.idx_of_element
    var i: Int = (idx).toInt()
    while (true) {
        var left: Int = (get_left_child_idx(i)).toInt()
        var right: Int = (get_right_child_idx(i)).toInt()
        var smallest: Int = (i).toInt()
        if ((left < heap.size) && (heap[left]!!._val < heap[smallest]!!._val)) {
            smallest = left
        }
        if ((right < heap.size) && (heap[right]!!._val < heap[smallest]!!._val)) {
            smallest = right
        }
        if (smallest != i) {
            var tmp: Node = heap[i]!!
            _listSet(heap, i, heap[smallest]!!)
            _listSet(heap, smallest, tmp)
            (idx_map)[heap[i]!!.name] = i
            (idx_map)[heap[smallest]!!.name] = smallest
            i = smallest
        } else {
            break
        }
    }
    mh.heap = heap
    mh.idx_of_element = idx_map
}

fun sift_up(mh: MinHeap, idx: Int): Unit {
    var heap: MutableList<Node> = mh.heap
    var idx_map: MutableMap<String, Int> = mh.idx_of_element
    var i: Int = (idx).toInt()
    var p: Int = (get_parent_idx(i)).toInt()
    while ((p >= 0) && (heap[p]!!._val > heap[i]!!._val)) {
        var tmp: Node = heap[p]!!
        _listSet(heap, p, heap[i]!!)
        _listSet(heap, i, tmp)
        (idx_map)[heap[p]!!.name] = p
        (idx_map)[heap[i]!!.name] = i
        i = p
        p = get_parent_idx(i)
    }
    mh.heap = heap
    mh.idx_of_element = idx_map
}

fun new_min_heap(array: MutableList<Node>): MinHeap {
    var idx_map: MutableMap<String, Int> = mutableMapOf<String, Int>()
    var val_map: MutableMap<String, Int> = mutableMapOf<String, Int>()
    var heap: MutableList<Node> = array
    var i: Int = (0).toInt()
    while (i < array.size) {
        var n: Node = array[i]!!
        (idx_map)[n.name] = i
        (val_map)[n.name] = n._val
        i = i + 1
    }
    var mh: MinHeap = MinHeap(heap = heap, idx_of_element = idx_map, heap_dict = val_map)
    var start: Int = (get_parent_idx(array.size - 1)).toInt()
    while (start >= 0) {
        sift_down(mh, start)
        start = start - 1
    }
    return mh
}

fun peek(mh: MinHeap): Node {
    return (mh.heap)[0]!!
}

fun remove_min(mh: MinHeap): Node {
    var heap: MutableList<Node> = mh.heap
    var idx_map: MutableMap<String, Int> = mh.idx_of_element
    var val_map: MutableMap<String, Int> = mh.heap_dict
    var last_idx: Int = (heap.size - 1).toInt()
    var top: Node = heap[0]!!
    var last: Node = heap[last_idx]!!
    _listSet(heap, 0, last)
    (idx_map)[last.name] = 0
    heap = slice_without_last(heap)
    idx_map = remove_key(idx_map, top.name)
    val_map = remove_key(val_map, top.name)
    mh.heap = heap
    mh.idx_of_element = idx_map
    mh.heap_dict = val_map
    if (heap.size > 0) {
        sift_down(mh, 0)
    }
    return top
}

fun insert(mh: MinHeap, node: Node): Unit {
    var heap: MutableList<Node> = mh.heap
    var idx_map: MutableMap<String, Int> = mh.idx_of_element
    var val_map: MutableMap<String, Int> = mh.heap_dict
    heap = run { val _tmp = heap.toMutableList(); _tmp.add(node); _tmp }
    var idx: Int = (heap.size - 1).toInt()
    (idx_map)[node.name] = idx
    (val_map)[node.name] = node._val
    mh.heap = heap
    mh.idx_of_element = idx_map
    mh.heap_dict = val_map
    sift_up(mh, idx)
}

fun is_empty(mh: MinHeap): Boolean {
    return (mh.heap).size == 0
}

fun get_value(mh: MinHeap, key: String): Int {
    return (mh.heap_dict)[key] as Int
}

fun decrease_key(mh: MinHeap, node: Node, new_value: Int): Unit {
    var heap: MutableList<Node> = mh.heap
    var val_map: MutableMap<String, Int> = mh.heap_dict
    var idx_map: MutableMap<String, Int> = mh.idx_of_element
    var idx: Int = ((idx_map)[node.name] as Int).toInt()
    if (!(heap[idx]!!._val > new_value)) {
        panic("newValue must be less than current value")
    }
    node._val = new_value
    heap[idx]!!._val = new_value
    (val_map)[node.name] = new_value
    mh.heap = heap
    mh.heap_dict = val_map
    sift_up(mh, idx)
}

fun node_to_string(n: Node): String {
    return ((("Node(" + n.name) + ", ") + _numToStr(n._val)) + ")"
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Min Heap - before decrease key")
        for (n in my_min_heap.heap) {
            println(node_to_string(n))
        }
        println("Min Heap - After decrease key of node [B -> -17]")
        decrease_key(my_min_heap, b, 0 - 17)
        for (n in my_min_heap.heap) {
            println(node_to_string(n))
        }
        println(_numToStr(get_value(my_min_heap, "B")))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
