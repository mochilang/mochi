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

fun int_pow(base: Int, exp: Int): Int {
    var result: Int = 1
    var i: Int = 0
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun backtrack(target: Int, exp: Int, current: Int, current_sum: Int): Int {
    if (current_sum == target) {
        return 1
    }
    var p: Int = int_pow(current, exp)
    var count: Int = 0
    if ((current_sum + p) <= target) {
        count = count + backtrack(target, exp, current + 1, current_sum + p)
    }
    if (p < target) {
        count = count + backtrack(target, exp, current + 1, current_sum)
    }
    return count
}

fun solve(target: Int, exp: Int): Int {
    if (!(((((((1 <= target) && (target <= 1000) as Boolean)) && (2 <= exp) as Boolean)) && (exp <= 10)) as Boolean)) {
        println("Invalid input")
        return 0
    }
    return backtrack(target, exp, 1, 0)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(solve(13, 2))
        println(solve(10, 2))
        println(solve(10, 3))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
