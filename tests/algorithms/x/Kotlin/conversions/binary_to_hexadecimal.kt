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

fun strip_spaces(s: String): String {
    var start: Int = 0
    var end: BigInteger = ((s.length - 1).toBigInteger())
    while ((start < s.length) && (s[start].toString() == " ")) {
        start = start + 1
    }
    while ((end.compareTo((start).toBigInteger()) >= 0) && (s[(end).toInt()].toString() == " ")) {
        end = end.subtract((1).toBigInteger())
    }
    var res: String = ""
    var i: Int = start
    while ((i).toBigInteger().compareTo((end)) <= 0) {
        res = res + s[i].toString()
        i = i + 1
    }
    return res
}

fun repeat_char(ch: String, count: Int): String {
    var res: String = ""
    var i: Int = 0
    while (i < count) {
        res = res + ch
        i = i + 1
    }
    return res
}

fun slice(s: String, start: Int, end: Int): String {
    var res: String = ""
    var i: Int = start
    while (i < end) {
        res = res + s[i].toString()
        i = i + 1
    }
    return res
}

fun bits_to_int(bits: String): Int {
    var value: Int = 0
    var i: Int = 0
    while (i < bits.length) {
        value = value * 2
        if (bits[i].toString() == "1") {
            value = value + 1
        }
        i = i + 1
    }
    return value
}

fun bin_to_hexadecimal(binary_str: String): String {
    var s: String = strip_spaces(binary_str)
    if (s.length == 0) {
        panic("Empty string was passed to the function")
    }
    var is_negative: Boolean = false
    if (s[0].toString() == "-") {
        is_negative = true
        s = s.substring(1, s.length)
    }
    var i: Int = 0
    while (i < s.length) {
        var c: String = s[i].toString()
        if ((c != "0") && (c != "1")) {
            panic("Non-binary value was passed to the function")
        }
        i = i + 1
    }
    var groups: Int = (s.length / 4) + 1
    var pad_len: Int = (groups * 4) - s.length
    s = repeat_char("0", pad_len) + s
    var digits: String = "0123456789abcdef"
    var res: String = "0x"
    var j: Int = 0
    while (j < s.length) {
        var chunk: String = s.substring(j, j + 4)
        var _val: Int = bits_to_int(chunk)
        res = res + digits[_val].toString()
        j = j + 4
    }
    if ((is_negative as Boolean)) {
        return "-" + res
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(bin_to_hexadecimal("101011111"))
        println(bin_to_hexadecimal(" 1010   "))
        println(bin_to_hexadecimal("-11101"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
