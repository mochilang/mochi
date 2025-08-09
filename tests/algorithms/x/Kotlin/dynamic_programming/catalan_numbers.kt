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

fun panic(msg: String): Nothing {
    println(msg)
    throw RuntimeException(msg)
}

fun catalan_numbers(upper_limit: Int): MutableList<Int> {
    if (upper_limit < 0) {
        panic("Limit for the Catalan sequence must be >= 0")
        return mutableListOf<Int>()
    }
    var catalans: MutableList<Int> = mutableListOf(1)
    var n: Int = (1).toInt()
    while (n <= upper_limit) {
        var next_val: Int = (0).toInt()
        var j: Int = (0).toInt()
        while (j < n) {
            next_val = next_val + (catalans[j]!! * catalans[(n - j) - 1]!!)
            j = j + 1
        }
        catalans = run { val _tmp = catalans.toMutableList(); _tmp.add(next_val); _tmp }
        n = n + 1
    }
    return catalans
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(catalan_numbers(5).toString())
        println(catalan_numbers(2).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
