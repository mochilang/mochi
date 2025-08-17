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

fun factorial(n: Int): Int {
    if (n < 0) {
        panic("factorial() not defined for negative values")
    }
    var value: Int = (1).toInt()
    var i: Int = (1).toInt()
    while (i <= n) {
        value = value * i
        i = i + 1
    }
    return value
}

fun factorial_recursive(n: Int): Int {
    if (n < 0) {
        panic("factorial() not defined for negative values")
    }
    if (n <= 1) {
        return 1
    }
    return n * factorial_recursive(n - 1)
}

fun test_zero(): Unit {
    if (factorial(0) != 1) {
        panic("factorial(0) failed")
    }
    if (factorial_recursive(0) != 1) {
        panic("factorial_recursive(0) failed")
    }
}

fun test_positive_integers(): Unit {
    if (factorial(1) != 1) {
        panic("factorial(1) failed")
    }
    if (factorial_recursive(1) != 1) {
        panic("factorial_recursive(1) failed")
    }
    if (factorial(5) != 120) {
        panic("factorial(5) failed")
    }
    if (factorial_recursive(5) != 120) {
        panic("factorial_recursive(5) failed")
    }
    if (factorial(7) != 5040) {
        panic("factorial(7) failed")
    }
    if (factorial_recursive(7) != 5040) {
        panic("factorial_recursive(7) failed")
    }
}

fun test_large_number(): Unit {
    if (factorial(10) != 3628800) {
        panic("factorial(10) failed")
    }
    if (factorial_recursive(10) != 3628800) {
        panic("factorial_recursive(10) failed")
    }
}

fun run_tests(): Unit {
    test_zero()
    test_positive_integers()
    test_large_number()
}

fun user_main(): Unit {
    run_tests()
    println(factorial(6))
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
