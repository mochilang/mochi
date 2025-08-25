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

data class CircularLinkedList(var data: MutableList<Int> = mutableListOf<Int>())
data class DeleteResult(var list: CircularLinkedList = CircularLinkedList(data = mutableListOf<Int>()), var value: Int = 0)
fun empty_list(): CircularLinkedList {
    return CircularLinkedList(data = mutableListOf<Int>())
}

fun length(list: CircularLinkedList): Int {
    return (list.data).size
}

fun is_empty(list: CircularLinkedList): Boolean {
    return (list.data).size == 0
}

fun to_string(list: CircularLinkedList): String {
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

fun insert_nth(list: CircularLinkedList, index: Int, value: Int): CircularLinkedList {
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
    return CircularLinkedList(data = res)
}

fun insert_head(list: CircularLinkedList, value: Int): CircularLinkedList {
    return insert_nth(list, 0, value)
}

fun insert_tail(list: CircularLinkedList, value: Int): CircularLinkedList {
    return insert_nth(list, (list.data).size, value)
}

fun delete_nth(list: CircularLinkedList, index: Int): DeleteResult {
    if ((index < 0) || (index >= (list.data).size)) {
        panic("index out of range")
    }
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    var _val: Int = (0).toInt()
    while (i < (list.data).size) {
        if (i == index) {
            _val = (list.data)[i]!!
        } else {
            res = run { val _tmp = res.toMutableList(); _tmp.add((list.data)[i]!!); _tmp }
        }
        i = i + 1
    }
    return DeleteResult(list = CircularLinkedList(data = res), value = _val)
}

fun delete_front(list: CircularLinkedList): DeleteResult {
    return delete_nth(list, 0)
}

fun delete_tail(list: CircularLinkedList): DeleteResult {
    return delete_nth(list, (list.data).size - 1)
}

fun user_main(): Unit {
    var cll: CircularLinkedList = empty_list()
    var i: Int = (0).toInt()
    while (i < 5) {
        cll = insert_tail(cll, i + 1)
        i = i + 1
    }
    println(to_string(cll))
    cll = insert_tail(cll, 6)
    println(to_string(cll))
    cll = insert_head(cll, 0)
    println(to_string(cll))
    var res: DeleteResult = delete_front(cll)
    cll = res.list
    println(res.value)
    res = delete_tail(cll)
    cll = res.list
    println(res.value)
    res = delete_nth(cll, 2)
    cll = res.list
    println(res.value)
    println(to_string(cll))
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
