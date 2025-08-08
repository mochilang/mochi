import java.math.BigInteger

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

fun decimal_to_binary_iterative(num: Int): String {
    if (num == 0) {
        return "0b0"
    }
    var negative: Boolean = false
    var n: Int = num
    if (n < 0) {
        negative = true
        n = 0 - n
    }
    var result: String = ""
    while (n > 0) {
        result = (Math.floorMod(n, 2)).toString() + result
        n = n / 2
    }
    if ((negative as Boolean)) {
        return "-0b" + result
    }
    return "0b" + result
}

fun decimal_to_binary_recursive_helper(n: Int): String {
    if (n == 0) {
        return "0"
    }
    if (n == 1) {
        return "1"
    }
    var div: Int = n / 2
    var mod: Int = Math.floorMod(n, 2)
    return decimal_to_binary_recursive_helper(div) + mod.toString()
}

fun decimal_to_binary_recursive(num: Int): String {
    if (num == 0) {
        return "0b0"
    }
    if (num < 0) {
        return "-0b" + decimal_to_binary_recursive_helper(0 - num)
    }
    return "0b" + decimal_to_binary_recursive_helper(num)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(decimal_to_binary_iterative(0))
        println(decimal_to_binary_iterative(2))
        println(decimal_to_binary_iterative(7))
        println(decimal_to_binary_iterative(35))
        println(decimal_to_binary_iterative(0 - 2))
        println(decimal_to_binary_recursive(0))
        println(decimal_to_binary_recursive(40))
        println(decimal_to_binary_recursive(0 - 40))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
