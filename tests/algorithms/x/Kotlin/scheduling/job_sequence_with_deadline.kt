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

fun max_tasks(tasks_info: MutableList<MutableList<Int>>): MutableList<Int> {
    var order: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < tasks_info.size) {
        order = run { val _tmp = order.toMutableList(); _tmp.add(i); _tmp }
        i = i + 1
    }
    var n: Int = (order.size).toInt()
    i = 0
    while (i < n) {
        var j: BigInteger = ((i + 1).toBigInteger())
        while (j.compareTo((n).toBigInteger()) < 0) {
            if ((((tasks_info[order[(j).toInt()]!!]!!) as MutableList<Int>))[1]!! > (((tasks_info[order[i]!!]!!) as MutableList<Int>))[1]!!) {
                var tmp: Int = (order[i]!!).toInt()
                _listSet(order, i, order[(j).toInt()]!!)
                _listSet(order, (j).toInt(), tmp)
            }
            j = j.add((1).toBigInteger())
        }
        i = i + 1
    }
    var result: MutableList<Int> = mutableListOf<Int>()
    var pos: Int = (1).toInt()
    i = 0
    while (i < n) {
        var id: Int = (order[i]!!).toInt()
        var deadline: Int = ((((tasks_info[id]!!) as MutableList<Int>))[0]!!).toInt()
        if (deadline >= pos) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(id); _tmp }
        }
        i = i + 1
        pos = pos + 1
    }
    return result
}

fun user_main(): Unit {
    var ex1: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(4, 20), mutableListOf(1, 10), mutableListOf(1, 40), mutableListOf(1, 30))
    var ex2: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 10), mutableListOf(2, 20), mutableListOf(3, 30), mutableListOf(2, 40))
    println(max_tasks(ex1).toString())
    println(max_tasks(ex2).toString())
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
