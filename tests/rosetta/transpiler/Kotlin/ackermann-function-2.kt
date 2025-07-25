var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Int {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed.toInt())
    } else {
        kotlin.math.abs(System.nanoTime().toInt())
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

fun pow(base: Int, exp: Int): Int {
    var result: Int = 1
    var i: Int = 0
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun ackermann2(m: Int, n: Int): Int {
    if (m == 0) {
        return n + 1
    }
    if (m == 1) {
        return n + 2
    }
    if (m == 2) {
        return (2 * n) + 3
    }
    if (m == 3) {
        return (8 * pow(2, n)) - 3
    }
    if (n == 0) {
        return ackermann2(m - 1, 1)
    }
    return ackermann2(m - 1, ackermann2(m, n - 1))
}

fun user_main(): Unit {
    println("A(0, 0) = " + ackermann2(0, 0).toString())
    println("A(1, 2) = " + ackermann2(1, 2).toString())
    println("A(2, 4) = " + ackermann2(2, 4).toString())
    println("A(3, 4) = " + ackermann2(3, 4).toString())
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
