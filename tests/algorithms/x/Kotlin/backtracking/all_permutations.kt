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

var sequence: MutableList<Any?> = mutableListOf(3, 1, 2, 4)
fun repeat_bool(times: Int): MutableList<Boolean> {
    var res: MutableList<Boolean> = mutableListOf<Boolean>()
    var i: Int = 0
    while (i < times) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(false); _tmp }
        i = i + 1
    }
    return res
}

fun set_bool(xs: MutableList<Boolean>, idx: Int, value: Boolean): MutableList<Boolean> {
    var res: MutableList<Boolean> = mutableListOf<Boolean>()
    var i: Int = 0
    while (i < xs.size) {
        if (i == idx) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(value); _tmp }
        } else {
            res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        }
        i = i + 1
    }
    return res
}

fun create_state_space_tree(sequence: MutableList<Any?>, current: MutableList<Any?>, index: Int, used: MutableList<Boolean>): Unit {
    if (index == sequence.size) {
        println(current.toString())
        return
    }
    var i: Int = 0
    while (i < sequence.size) {
        if (!((used[i]!!) as? Boolean ?: false)) {
            var next_current: MutableList<Any?> = run { val _tmp = current.toMutableList(); _tmp.add(sequence[i] as Any?); _tmp }
            var next_used: MutableList<Boolean> = set_bool(used, i, true)
            create_state_space_tree(sequence, next_current, index + 1, next_used)
        }
        i = i + 1
    }
}

fun generate_all_permutations(sequence: MutableList<Any?>): Unit {
    var used: MutableList<Boolean> = repeat_bool(sequence.size)
    create_state_space_tree(sequence, mutableListOf<Any?>(), 0, used)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        generate_all_permutations(sequence)
        var sequence_2: MutableList<Any?> = mutableListOf("A", "B", "C")
        generate_all_permutations(sequence_2)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
