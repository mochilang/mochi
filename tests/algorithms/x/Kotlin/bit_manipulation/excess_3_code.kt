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

fun excess_3_code(number: Int): String {
    var n: Int = number
    if (n < 0) {
        n = 0
    }
    var mapping: MutableList<String> = mutableListOf("0011", "0100", "0101", "0110", "0111", "1000", "1001", "1010", "1011", "1100")
    var res: String = ""
    if (n == 0) {
        res = mapping[0]!!
    } else {
        while (n > 0) {
            var digit: Int = Math.floorMod(n, 10)
            res = mapping[digit]!! + res
            n = n / 10
        }
    }
    return "0b" + res
}

fun user_main(): Unit {
    println(excess_3_code(0))
    println(excess_3_code(3))
    println(excess_3_code(2))
    println(excess_3_code(20))
    println(excess_3_code(120))
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
