fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

fun double_factorial_recursive(n: Int): Int {
    if (n < 0) {
        panic("double_factorial_recursive() not defined for negative values")
    }
    if (n <= 1) {
        return 1
    }
    return n * double_factorial_recursive(n - 2)
}

fun double_factorial_iterative(n: Int): Int {
    if (n < 0) {
        panic("double_factorial_iterative() not defined for negative values")
    }
    var result: Int = (1).toInt()
    var i: Int = (n).toInt()
    while (i > 0) {
        result = result * i
        i = i - 2
    }
    return result
}

fun test_double_factorial(): Unit {
    if (double_factorial_recursive(0) != 1) {
        panic("0!! recursive failed")
    }
    if (double_factorial_iterative(0) != 1) {
        panic("0!! iterative failed")
    }
    if (double_factorial_recursive(1) != 1) {
        panic("1!! recursive failed")
    }
    if (double_factorial_iterative(1) != 1) {
        panic("1!! iterative failed")
    }
    if (double_factorial_recursive(5) != 15) {
        panic("5!! recursive failed")
    }
    if (double_factorial_iterative(5) != 15) {
        panic("5!! iterative failed")
    }
    if (double_factorial_recursive(6) != 48) {
        panic("6!! recursive failed")
    }
    if (double_factorial_iterative(6) != 48) {
        panic("6!! iterative failed")
    }
    var n: Int = (0).toInt()
    while (n <= 10) {
        if (double_factorial_recursive(n) != double_factorial_iterative(n)) {
            panic("double factorial mismatch")
        }
        n = n + 1
    }
}

fun user_main(): Unit {
    test_double_factorial()
    println(double_factorial_iterative(10))
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
