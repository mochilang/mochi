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

fun backtrack(candidates: MutableList<Int>, start: Int, target: Int, path: MutableList<Int>, result: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var result: MutableList<MutableList<Int>> = result
    if (target == 0) {
        return run { val _tmp = result.toMutableList(); _tmp.add(path); _tmp }
    }
    var i: Int = start
    while (i < candidates.size) {
        var value: Int = candidates[i]!!
        if (value <= target) {
            var new_path = run { val _tmp = path.toMutableList(); _tmp.add(value); _tmp }
            result = backtrack(candidates, i, target - value, (new_path as MutableList<Int>), result)
        }
        i = i + 1
    }
    return result
}

fun combination_sum(candidates: MutableList<Int>, target: Int): MutableList<MutableList<Int>> {
    var path: MutableList<Int> = mutableListOf<Int>()
    var result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    return backtrack(candidates, 0, target, path, result)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(combination_sum(mutableListOf(2, 3, 5), 8).toString())
        println(combination_sum(mutableListOf(2, 3, 6, 7), 7).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
