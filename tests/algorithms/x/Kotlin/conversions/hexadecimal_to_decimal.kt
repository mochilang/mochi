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

fun strip(s: String): String {
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

fun hex_digit_value(c: String): Int {
    if (c == "0") {
        return 0
    }
    if (c == "1") {
        return 1
    }
    if (c == "2") {
        return 2
    }
    if (c == "3") {
        return 3
    }
    if (c == "4") {
        return 4
    }
    if (c == "5") {
        return 5
    }
    if (c == "6") {
        return 6
    }
    if (c == "7") {
        return 7
    }
    if (c == "8") {
        return 8
    }
    if (c == "9") {
        return 9
    }
    if ((c == "a") || (c == "A")) {
        return 10
    }
    if ((c == "b") || (c == "B")) {
        return 11
    }
    if ((c == "c") || (c == "C")) {
        return 12
    }
    if ((c == "d") || (c == "D")) {
        return 13
    }
    if ((c == "e") || (c == "E")) {
        return 14
    }
    if ((c == "f") || (c == "F")) {
        return 15
    }
    println("Non-hexadecimal value was passed to the function")
    return 0
}

fun hex_to_decimal(hex_string: String): Int {
    var s: String = strip(hex_string)
    if (s.length == 0) {
        println("Empty string was passed to the function")
        return 0
    }
    var is_negative: Boolean = false
    if (s.substring(0, 1) == "-") {
        is_negative = true
        s = s.substring(1, s.length)
    }
    var decimal_number: Int = 0
    var i: Int = 0
    while (i < s.length) {
        var c: String = s.substring(i, i + 1)
        var value: Int = hex_digit_value(c)
        decimal_number = (16 * decimal_number) + value
        i = i + 1
    }
    if ((is_negative as Boolean)) {
        return 0 - decimal_number
    }
    return decimal_number
}

fun user_main(): Unit {
    println(hex_to_decimal("a").toString())
    println(hex_to_decimal("12f").toString())
    println(hex_to_decimal("   12f   ").toString())
    println(hex_to_decimal("FfFf").toString())
    println(hex_to_decimal("-Ff").toString())
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
