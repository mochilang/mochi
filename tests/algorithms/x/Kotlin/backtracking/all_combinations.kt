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

fun create_all_state(increment: Int, total: Int, level: Int, current: MutableList<Int>, result: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var result: MutableList<MutableList<Int>> = result
    if (level == 0) {
        return run { val _tmp = result.toMutableList(); _tmp.add(current); _tmp }
    }
    var i: Int = increment
    while (i <= ((total - level) + 1)) {
        var next_current = run { val _tmp = current.toMutableList(); _tmp.add(i); _tmp }
        result = create_all_state(i + 1, total, level - 1, (next_current as MutableList<Int>), result)
        i = i + 1
    }
    return result
}

fun generate_all_combinations(n: Int, k: Int): MutableList<MutableList<Int>> {
    if ((k < 0) || (n < 0)) {
        return mutableListOf<MutableList<Int>>()
    }
    var result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    return create_all_state(1, n, k, mutableListOf<Int>(), result)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(generate_all_combinations(4, 2).toString())
        println(generate_all_combinations(3, 1).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
