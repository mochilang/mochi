val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/linked_list"

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

data class DoublyLinkedList(var data: MutableList<Int> = mutableListOf<Int>())
data class DeleteResult(var list: DoublyLinkedList = DoublyLinkedList(data = mutableListOf<Int>()), var value: Int = 0)
fun empty_list(): DoublyLinkedList {
    return DoublyLinkedList(data = mutableListOf<Int>())
}

fun length(list: DoublyLinkedList): Int {
    return (list.data).size
}

fun is_empty(list: DoublyLinkedList): Boolean {
    return (list.data).size == 0
}

fun to_string(list: DoublyLinkedList): String {
    if ((list.data).size == 0) {
        return ""
    }
    var s: String = _numToStr((list.data)[0]!!)
    var i: Int = (1).toInt()
    while (i < (list.data).size) {
        s = (s + "->") + _numToStr((list.data)[i]!!)
        i = i + 1
    }
    return s
}

fun insert_nth(list: DoublyLinkedList, index: Int, value: Int): DoublyLinkedList {
    if ((index < 0) || (index > (list.data).size)) {
        panic("index out of range")
    }
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < index) {
        res = run { val _tmp = res.toMutableList(); _tmp.add((list.data)[i]!!); _tmp }
        i = i + 1
    }
    res = run { val _tmp = res.toMutableList(); _tmp.add(value); _tmp }
    while (i < (list.data).size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add((list.data)[i]!!); _tmp }
        i = i + 1
    }
    return DoublyLinkedList(data = res)
}

fun insert_head(list: DoublyLinkedList, value: Int): DoublyLinkedList {
    return insert_nth(list, 0, value)
}

fun insert_tail(list: DoublyLinkedList, value: Int): DoublyLinkedList {
    return insert_nth(list, (list.data).size, value)
}

fun delete_nth(list: DoublyLinkedList, index: Int): DeleteResult {
    if ((index < 0) || (index >= (list.data).size)) {
        panic("index out of range")
    }
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    var removed: Int = (0).toInt()
    while (i < (list.data).size) {
        if (i == index) {
            removed = (list.data)[i]!!
        } else {
            res = run { val _tmp = res.toMutableList(); _tmp.add((list.data)[i]!!); _tmp }
        }
        i = i + 1
    }
    return DeleteResult(list = DoublyLinkedList(data = res), value = removed)
}

fun delete_head(list: DoublyLinkedList): DeleteResult {
    return delete_nth(list, 0)
}

fun delete_tail(list: DoublyLinkedList): DeleteResult {
    return delete_nth(list, (list.data).size - 1)
}

fun delete_value(list: DoublyLinkedList, value: Int): DeleteResult {
    var idx: Int = (0).toInt()
    var found: Boolean = false
    while (idx < (list.data).size) {
        if ((list.data)[idx]!! == value) {
            found = true
            break
        }
        idx = idx + 1
    }
    if (!found) {
        panic("value not found")
    }
    return delete_nth(list, idx)
}

fun user_main(): Unit {
    var dll: DoublyLinkedList = empty_list()
    dll = insert_tail(dll, 1)
    dll = insert_tail(dll, 2)
    dll = insert_tail(dll, 3)
    println(to_string(dll))
    dll = insert_head(dll, 0)
    println(to_string(dll))
    dll = insert_nth(dll, 2, 9)
    println(to_string(dll))
    var res: DeleteResult = delete_nth(dll, 2)
    dll = res.list
    println(res.value)
    println(to_string(dll))
    res = delete_tail(dll)
    dll = res.list
    println(res.value)
    println(to_string(dll))
    res = delete_value(dll, 1)
    dll = res.list
    println(res.value)
    println(to_string(dll))
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
