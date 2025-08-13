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

fun make_list(len: Int, value: Int): MutableList<Int> {
    var arr: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < len) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(value); _tmp }
        i = i + 1
    }
    return arr
}

fun min_int(a: Int, b: Int): Int {
    if (a < b) {
        return a
    }
    return b
}

fun min_steps_to_one(number: Int): Int {
    if (number <= 0) {
        return 0
    }
    var table: MutableList<Int> = make_list(number + 1, number + 1)
    _listSet(table, 1, 0)
    var i: Int = (1).toInt()
    while (i < number) {
        _listSet(table, i + 1, min_int(table[i + 1]!!, table[i]!! + 1))
        if ((i * 2) <= number) {
            _listSet(table, i * 2, min_int(table[i * 2]!!, table[i]!! + 1))
        }
        if ((i * 3) <= number) {
            _listSet(table, i * 3, min_int(table[i * 3]!!, table[i]!! + 1))
        }
        i = i + 1
    }
    return table[number]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(min_steps_to_one(10).toString())
        println(min_steps_to_one(15).toString())
        println(min_steps_to_one(6).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
