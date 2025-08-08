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

fun bin_to_octal(bin_string: String): String {
    var i: Int = 0
    while (i < bin_string.length) {
        var c: String = bin_string[i].toString()
        if (!(((c == "0") || (c == "1")) as Boolean)) {
            panic("Non-binary value was passed to the function")
        }
        i = i + 1
    }
    if (bin_string.length == 0) {
        panic("Empty string was passed to the function")
    }
    var padded: String = bin_string
    while ((Math.floorMod(padded.length, 3)) != 0) {
        padded = "0" + padded
    }
    var oct_string: String = ""
    var index: Int = 0
    while (index < padded.length) {
        var group: String = padded.substring(index, index + 3)
        var b0: Int = (if (group[0].toString() == "1") 1 else 0 as Int)
        var b1: Int = (if (group[1].toString() == "1") 1 else 0 as Int)
        var b2: Int = (if (group[2].toString() == "1") 1 else 0 as Int)
        var oct_val: Int = ((b0 * 4) + (b1 * 2)) + b2
        oct_string = oct_string + oct_val.toString()
        index = index + 3
    }
    return oct_string
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(bin_to_octal("1111"))
        println(bin_to_octal("101010101010011"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
