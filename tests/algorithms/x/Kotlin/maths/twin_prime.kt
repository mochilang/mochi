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

fun is_prime(n: Int): Boolean {
    if (n < 2) {
        return false
    }
    if ((Math.floorMod(n, 2)) == 0) {
        return n == 2
    }
    var i: Int = (3).toInt()
    while ((i * i) <= n) {
        if ((Math.floorMod(n, i)) == 0) {
            return false
        }
        i = i + 2
    }
    return true
}

fun twin_prime(number: Int): Int {
    if (is_prime(number) && is_prime(number + 2)) {
        return number + 2
    }
    return 0 - 1
}

fun test_twin_prime(): Unit {
    if (twin_prime(3) != 5) {
        panic("twin_prime(3) failed")
    }
    if (twin_prime(4) != (0 - 1)) {
        panic("twin_prime(4) failed")
    }
    if (twin_prime(5) != 7) {
        panic("twin_prime(5) failed")
    }
    if (twin_prime(17) != 19) {
        panic("twin_prime(17) failed")
    }
    if (twin_prime(0) != (0 - 1)) {
        panic("twin_prime(0) failed")
    }
}

fun user_main(): Unit {
    test_twin_prime()
    println(twin_prime(3))
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
