import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/linked_list"

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

data class LinkedList(var next: MutableList<Int> = mutableListOf<Int>(), var head: Int = 0)
var NULL: Int = (0 - 1).toInt()
fun empty_list(): LinkedList {
    return LinkedList(next = mutableListOf<Int>(), head = NULL.toInt())
}

fun add_node(list: LinkedList, value: Int): LinkedList {
    var nexts: MutableList<Int> = list.next
    var new_index: Int = (nexts.size).toInt()
    nexts = run { val _tmp = nexts.toMutableList(); _tmp.add(NULL.toInt()); _tmp }
    if ((list.head).toBigInteger().compareTo((NULL)) == 0) {
        return LinkedList(next = nexts, head = new_index)
    }
    var last: Int = (list.head).toInt()
    while ((nexts[last]!!).toBigInteger().compareTo((NULL)) != 0) {
        last = nexts[last]!!
    }
    var new_nexts: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < nexts.size) {
        if (i == last) {
            new_nexts = run { val _tmp = new_nexts.toMutableList(); _tmp.add(new_index); _tmp }
        } else {
            new_nexts = run { val _tmp = new_nexts.toMutableList(); _tmp.add(nexts[i]!!); _tmp }
        }
        i = i + 1
    }
    return LinkedList(next = new_nexts, head = list.head)
}

fun set_next(list: LinkedList, index: Int, next_index: Int): LinkedList {
    var nexts: MutableList<Int> = list.next
    var new_nexts: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < nexts.size) {
        if (i == index) {
            new_nexts = run { val _tmp = new_nexts.toMutableList(); _tmp.add(next_index); _tmp }
        } else {
            new_nexts = run { val _tmp = new_nexts.toMutableList(); _tmp.add(nexts[i]!!); _tmp }
        }
        i = i + 1
    }
    return LinkedList(next = new_nexts, head = list.head)
}

fun detect_cycle(list: LinkedList): Boolean {
    if ((list.head).toBigInteger().compareTo((NULL)) == 0) {
        return false
    }
    var nexts: MutableList<Int> = list.next
    var slow: Int = (list.head).toInt()
    var fast: Int = (list.head).toInt()
    while (((fast).toBigInteger().compareTo((NULL)) != 0) && ((nexts[fast]!!).toBigInteger().compareTo((NULL)) != 0)) {
        slow = nexts[slow]!!
        fast = nexts[nexts[fast]!!]!!
        if (slow == fast) {
            return true
        }
    }
    return false
}

fun user_main(): Unit {
    var ll: LinkedList = empty_list()
    ll = add_node(ll, 1)
    ll = add_node(ll, 2)
    ll = add_node(ll, 3)
    ll = add_node(ll, 4)
    ll = set_next(ll, 3, 1)
    println(detect_cycle(ll))
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
