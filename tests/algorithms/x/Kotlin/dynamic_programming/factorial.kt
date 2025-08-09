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

var memo: MutableList<Int> = mutableListOf(1, 1)
fun factorial(num: Int): Int {
    if (num < 0) {
        println("Number should not be negative.")
        return 0
    }
    var m: MutableList<Int> = memo
    var i: Int = (m.size).toInt()
    while (i <= num) {
        m = run { val _tmp = m.toMutableList(); _tmp.add(i * m[i - 1]!!); _tmp }
        i = i + 1
    }
    memo = m
    return m[num]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(factorial(7).toString())
        factorial(0 - 1)
        var results: MutableList<Int> = mutableListOf<Int>()
        for (i in 0 until 10) {
            results = run { val _tmp = results.toMutableList(); _tmp.add(factorial(i)); _tmp }
        }
        println(results.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
