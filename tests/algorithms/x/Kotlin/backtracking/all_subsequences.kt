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

var seq: MutableList<Any?> = mutableListOf(1, 2, 3)
fun create_state_space_tree(sequence: MutableList<Any?>, current: MutableList<Any?>, index: Int): Unit {
    if (index == sequence.size) {
        println(current)
        return
    }
    create_state_space_tree(sequence, current, index + 1)
    var with_elem: MutableList<Any?> = run { val _tmp = current.toMutableList(); _tmp.add(sequence[index] as Any?); _tmp }
    create_state_space_tree(sequence, with_elem, index + 1)
}

fun generate_all_subsequences(sequence: MutableList<Any?>): Unit {
    create_state_space_tree(sequence, mutableListOf<Any?>(), 0)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        generate_all_subsequences(seq)
        var seq2: MutableList<Any?> = mutableListOf("A", "B", "C")
        generate_all_subsequences(seq2)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
