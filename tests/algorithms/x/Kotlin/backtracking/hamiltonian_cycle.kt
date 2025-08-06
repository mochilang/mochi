fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }

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

fun valid_connection(graph: MutableList<MutableList<Int>>, next_ver: Int, curr_ind: Int, path: MutableList<Int>): Boolean {
    if ((((graph[path[curr_ind - 1]!!]!!) as MutableList<Int>))[next_ver]!! == 0) {
        return false
    }
    for (v in path) {
        if (v == next_ver) {
            return false
        }
    }
    return true
}

fun util_hamilton_cycle(graph: MutableList<MutableList<Int>>, path: MutableList<Int>, curr_ind: Int): Boolean {
    if (curr_ind == graph.size) {
        return (((graph[path[curr_ind - 1]!!]!!) as MutableList<Int>))[path[0]!!]!! == 1
    }
    var next_ver: Int = 0
    while (next_ver < graph.size) {
        if (((valid_connection(graph, next_ver, curr_ind, path)) as Boolean)) {
            _listSet(path, curr_ind, next_ver)
            if (((util_hamilton_cycle(graph, path, curr_ind + 1)) as Boolean)) {
                return true
            }
            _listSet(path, curr_ind, 0 - 1)
        }
        next_ver = next_ver + 1
    }
    return false
}

fun hamilton_cycle(graph: MutableList<MutableList<Int>>, start_index: Int): MutableList<Int> {
    var path: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < (graph.size + 1)) {
        _listSet(path, i, 0 - 1)
        i = i + 1
    }
    _listSet(path, 0, start_index)
    var last: Int = path.size - 1
    _listSet(path, last, start_index)
    if (((util_hamilton_cycle(graph, path, 1)) as Boolean)) {
        return path
    }
    return mutableListOf<Int>()
}

fun test_case_1(): Unit {
    var graph: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 1, 0, 1, 0), mutableListOf(1, 0, 1, 1, 1), mutableListOf(0, 1, 0, 0, 1), mutableListOf(1, 1, 0, 0, 1), mutableListOf(0, 1, 1, 1, 0))
    expect(hamilton_cycle(graph, 0) == mutableListOf(0, 1, 2, 4, 3, 0))
}

fun test_case_2(): Unit {
    var graph: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 1, 0, 1, 0), mutableListOf(1, 0, 1, 1, 1), mutableListOf(0, 1, 0, 0, 1), mutableListOf(1, 1, 0, 0, 1), mutableListOf(0, 1, 1, 1, 0))
    expect(hamilton_cycle(graph, 3) == mutableListOf(3, 0, 1, 2, 4, 3))
}

fun test_case_3(): Unit {
    var graph: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 1, 0, 1, 0), mutableListOf(1, 0, 1, 1, 1), mutableListOf(0, 1, 0, 0, 1), mutableListOf(1, 1, 0, 0, 0), mutableListOf(0, 1, 1, 0, 0))
    expect(hamilton_cycle(graph, 4) == mutableListOf<Any?>())
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        test_case_1()
        test_case_2()
        test_case_3()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
