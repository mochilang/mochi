import java.math.BigInteger

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

fun get_reverse_bit_string(number: Int): String {
    var bit_string: String = ""
    var n: Int = number
    var i: Int = 0
    while (i < 32) {
        bit_string = bit_string + (Math.floorMod(n, 2)).toString()
        n = n / 2
        i = i + 1
    }
    return bit_string
}

fun reverse_bit(number: Int): String {
    if (number < 0) {
        panic("the value of input must be positive")
    }
    var n: Int = number
    var result: Int = 0
    var i: Int = 1
    while (i <= 32) {
        result = result * 2
        var end_bit: Int = Math.floorMod(n, 2)
        n = n / 2
        result = result + end_bit
        i = i + 1
    }
    return get_reverse_bit_string(result)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(reverse_bit(25))
        println(reverse_bit(37))
        println(reverse_bit(21))
        println(reverse_bit(58))
        println(reverse_bit(0))
        println(reverse_bit(256))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
