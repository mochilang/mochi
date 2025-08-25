import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/heap"

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

var heap: MutableList<Int> = mutableListOf(0)
var size: Int = (0).toInt()
fun swap_up(i: Int): Unit {
    var temp: Int = (heap[i]!!).toInt()
    var idx: Int = (i).toInt()
    while ((Math.floorDiv(idx, 2)) > 0) {
        if (heap[idx]!! > heap[Math.floorDiv(idx, 2)]!!) {
            _listSet(heap, idx, heap[Math.floorDiv(idx, 2)]!!)
            _listSet(heap, Math.floorDiv(idx, 2), temp)
        }
        idx = Math.floorDiv(idx, 2)
    }
}

fun insert(value: Int): Unit {
    heap = run { val _tmp = heap.toMutableList(); _tmp.add(value); _tmp }
    size = (size + 1).toInt()
    swap_up(size)
}

fun swap_down(i: Int): Unit {
    var idx: Int = (i).toInt()
    while (size >= (2 * idx)) {
        var bigger_child = if (((2 * idx) + 1) > size) 2 * idx else if (heap[2 * idx]!! > heap[(2 * idx) + 1]!!) 2 * idx else (2 * idx) + 1
        var temp: Int = (heap[idx]!!).toInt()
        if (heap[idx]!! < heap[(bigger_child).toInt()]!!) {
            _listSet(heap, idx, heap[(bigger_child).toInt()]!!)
            _listSet(heap, (bigger_child).toInt(), temp)
        }
        idx = bigger_child.toInt()
    }
}

fun shrink(): Unit {
    var new_heap: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i <= size) {
        new_heap = run { val _tmp = new_heap.toMutableList(); _tmp.add(heap[i]!!); _tmp }
        i = i + 1
    }
    heap = new_heap
}

fun pop(): Int {
    var max_value: Int = (heap[1]!!).toInt()
    _listSet(heap, 1, heap[size]!!)
    size = (size - 1).toInt()
    shrink()
    swap_down(1)
    return max_value
}

fun get_list(): MutableList<Int> {
    var out: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (1).toInt()
    while (i <= size) {
        out = run { val _tmp = out.toMutableList(); _tmp.add(heap[i]!!); _tmp }
        i = i + 1
    }
    return out
}

fun len(): Int {
    return size
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        insert(6)
        insert(10)
        insert(15)
        insert(12)
        println(pop())
        println(pop())
        println(get_list())
        println(len())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
