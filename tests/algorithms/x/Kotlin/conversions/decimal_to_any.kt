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

fun decimal_to_any(num: Int, base: Int): String {
    if (num < 0) {
        panic("parameter must be positive int")
    }
    if (base < 2) {
        panic("base must be >= 2")
    }
    if (base > 36) {
        panic("base must be <= 36")
    }
    if (num == 0) {
        return "0"
    }
    var symbols: String = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var n: Int = num
    var result: String = ""
    while (n > 0) {
        var mod: Int = Math.floorMod(n, base)
        var digit: String = symbols.substring(mod, mod + 1)
        result = digit + result
        n = n / base
    }
    return result
}

fun user_main(): Unit {
    println(decimal_to_any(0, 2))
    println(decimal_to_any(5, 4))
    println(decimal_to_any(20, 3))
    println(decimal_to_any(58, 16))
    println(decimal_to_any(243, 17))
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
