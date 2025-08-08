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

var values: MutableList<String> = mutableListOf("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f")
fun decimal_to_hexadecimal(decimal: Int): String {
    var num: Int = decimal
    var negative: Boolean = false
    if (num < 0) {
        negative = true
        num = 0 - num
    }
    if (num == 0) {
        if ((negative as Boolean)) {
            return "-0x0"
        }
        return "0x0"
    }
    var hex: String = ""
    while (num > 0) {
        var remainder: Int = Math.floorMod(num, 16)
        hex = values[remainder]!! + hex
        num = num / 16
    }
    if ((negative as Boolean)) {
        return "-0x" + hex
    }
    return "0x" + hex
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(decimal_to_hexadecimal(5))
        println(decimal_to_hexadecimal(15))
        println(decimal_to_hexadecimal(37))
        println(decimal_to_hexadecimal(255))
        println(decimal_to_hexadecimal(4096))
        println(decimal_to_hexadecimal(999098))
        println(decimal_to_hexadecimal(0 - 256))
        println(decimal_to_hexadecimal(0))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
