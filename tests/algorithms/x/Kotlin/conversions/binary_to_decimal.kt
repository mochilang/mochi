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

fun trim(s: String): String {
    var start: Int = 0
    while (start < s.length) {
        var ch: String = s.substring(start, start + 1)
        if ((((((ch != " ") && (ch != "\n") as Boolean)) && (ch != "\t") as Boolean)) && (ch != "\r")) {
            break
        }
        start = start + 1
    }
    var end: Int = s.length
    while (end > start) {
        var ch: String = s.substring(end - 1, end)
        if ((((((ch != " ") && (ch != "\n") as Boolean)) && (ch != "\t") as Boolean)) && (ch != "\r")) {
            break
        }
        end = end - 1
    }
    return s.substring(start, end)
}

fun bin_to_decimal(bin_string: String): Int {
    var trimmed: String = trim(bin_string)
    if (trimmed == "") {
        panic("Empty string was passed to the function")
    }
    var is_negative: Boolean = false
    var s: String = trimmed
    if (s.substring(0, 1) == "-") {
        is_negative = true
        s = s.substring(1, s.length)
    }
    var i: Int = 0
    while (i < s.length) {
        var c: String = s.substring(i, i + 1)
        if ((c != "0") && (c != "1")) {
            panic("Non-binary value was passed to the function")
        }
        i = i + 1
    }
    var decimal_number: Int = 0
    i = 0
    while (i < s.length) {
        var c: String = s.substring(i, i + 1)
        var digit: Int = (c.toInt())
        decimal_number = (2 * decimal_number) + digit
        i = i + 1
    }
    if ((is_negative as Boolean)) {
        return 0 - decimal_number
    }
    return decimal_number
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(bin_to_decimal("101").toString())
        println(bin_to_decimal(" 1010   ").toString())
        println(bin_to_decimal("-11101").toString())
        println(bin_to_decimal("0").toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
