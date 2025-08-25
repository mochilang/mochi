val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/maths/special_numbers"

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

fun int_sqrt(n: Int): Int {
    var r: Int = (0).toInt()
    while ((((r).toLong() + (1).toLong()) * ((r).toLong() + (1).toLong())) <= n) {
        r = ((r).toLong() + (1).toLong()).toInt()
    }
    return r
}

fun is_pronic(n: Int): Boolean {
    if (n < 0) {
        return false
    }
    if ((Math.floorMod(n, 2)) != 0) {
        return false
    }
    var root: Int = (int_sqrt(n)).toInt()
    return (n).toLong() == ((root).toLong() * ((root).toLong() + (1).toLong()))
}

fun test_is_pronic(): Unit {
    if ((is_pronic(0 - 1)) as Boolean) {
        panic("-1 should not be pronic")
    }
    if (!is_pronic(0)) {
        panic("0 should be pronic")
    }
    if (!is_pronic(2)) {
        panic("2 should be pronic")
    }
    if ((is_pronic(5)) as Boolean) {
        panic("5 should not be pronic")
    }
    if (!is_pronic(6)) {
        panic("6 should be pronic")
    }
    if ((is_pronic(8)) as Boolean) {
        panic("8 should not be pronic")
    }
    if (!is_pronic(30)) {
        panic("30 should be pronic")
    }
    if ((is_pronic(32)) as Boolean) {
        panic("32 should not be pronic")
    }
    if (!is_pronic(2147441940)) {
        panic("2147441940 should be pronic")
    }
}

fun user_main(): Unit {
    test_is_pronic()
    println(is_pronic(56))
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
