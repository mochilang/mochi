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

fun panic(msg: String): Nothing {
    println(msg)
    throw RuntimeException(msg)
}

fun trim_spaces(s: String): String {
    var start: Int = 0
    var end: Int = s.length
    while ((start < end) && (s.substring(start, start + 1) == " ")) {
        start = start + 1
    }
    while ((end > start) && (s.substring(end - 1, end) == " ")) {
        end = end - 1
    }
    return s.substring(start, end)
}

fun hex_digit_value(ch: String): Int {
    if (ch == "0") {
        return 0
    }
    if (ch == "1") {
        return 1
    }
    if (ch == "2") {
        return 2
    }
    if (ch == "3") {
        return 3
    }
    if (ch == "4") {
        return 4
    }
    if (ch == "5") {
        return 5
    }
    if (ch == "6") {
        return 6
    }
    if (ch == "7") {
        return 7
    }
    if (ch == "8") {
        return 8
    }
    if (ch == "9") {
        return 9
    }
    if ((ch == "a") || (ch == "A")) {
        return 10
    }
    if ((ch == "b") || (ch == "B")) {
        return 11
    }
    if ((ch == "c") || (ch == "C")) {
        return 12
    }
    if ((ch == "d") || (ch == "D")) {
        return 13
    }
    if ((ch == "e") || (ch == "E")) {
        return 14
    }
    if ((ch == "f") || (ch == "F")) {
        return 15
    }
    panic("Invalid value was passed to the function")
}

fun hex_to_bin(hex_num: String): Int {
    var trimmed: String = trim_spaces(hex_num)
    if (trimmed.length == 0) {
        panic("No value was passed to the function")
    }
    var s: String = trimmed
    var is_negative: Boolean = false
    if (s.substring(0, 1) == "-") {
        is_negative = true
        s = s.substring(1, s.length)
    }
    var int_num: Int = 0
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        var _val: Int = hex_digit_value(ch)
        int_num = (int_num * 16) + _val
        i = i + 1
    }
    var bin_str: String = ""
    var n: Int = int_num
    if (n == 0) {
        bin_str = "0"
    }
    while (n > 0) {
        bin_str = (Math.floorMod(n, 2)).toString() + bin_str
        n = n / 2
    }
    var result: Int = (bin_str.toBigInteger().toInt())
    if ((is_negative as Boolean)) {
        result = 0 - result
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(hex_to_bin("AC").toString())
        println(hex_to_bin("9A4").toString())
        println(hex_to_bin("   12f   ").toString())
        println(hex_to_bin("FfFf").toString())
        println(hex_to_bin("-fFfF").toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
