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

fun count_assignments(person: Int, task_performed: MutableList<MutableList<Int>>, used: MutableList<Int>): Int {
    if (person == task_performed.size) {
        return 1
    }
    var total: Int = (0).toInt()
    var tasks: MutableList<Int> = task_performed[person]!!
    var i: Int = (0).toInt()
    while (i < tasks.size) {
        var t: Int = (tasks[i]!!).toInt()
        if (!(t in used)) {
            total = total + count_assignments(person + 1, task_performed, run { val _tmp = used.toMutableList(); _tmp.add(t); _tmp })
        }
        i = i + 1
    }
    return total
}

fun count_no_of_ways(task_performed: MutableList<MutableList<Int>>): Int {
    return count_assignments(0, task_performed, mutableListOf<Int>())
}

fun user_main(): Unit {
    var task_performed: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 3, 4), mutableListOf(1, 2, 5), mutableListOf(3, 4))
    println(count_no_of_ways(task_performed).toString())
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
