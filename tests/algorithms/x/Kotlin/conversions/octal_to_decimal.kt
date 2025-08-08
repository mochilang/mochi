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

fun panic(msg: String): Nothing {
    println(msg)
    throw RuntimeException(msg)
}

fun trim_spaces(s: String): String {
    var start: Int = 0
    var end: BigInteger = ((s.length - 1).toBigInteger())
    while (((start).toBigInteger().compareTo((end)) <= 0) && (s.substring(start, start + 1) == " ")) {
        start = start + 1
    }
    while ((end.compareTo((start).toBigInteger()) >= 0) && (s.substring((end).toInt(), (end.add((1).toBigInteger())).toInt()) == " ")) {
        end = end.subtract((1).toBigInteger())
    }
    if ((start).toBigInteger().compareTo((end)) > 0) {
        return ""
    }
    return s.substring(start, (end.add((1).toBigInteger())).toInt())
}

fun char_to_digit(ch: String): Int {
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
    panic("Non-octal value was passed to the function")
    return 0
}

fun oct_to_decimal(oct_string: String): Int {
    var s: String = trim_spaces(oct_string)
    if (s.length == 0) {
        panic("Empty string was passed to the function")
        return 0
    }
    var is_negative: Boolean = false
    if (s.substring(0, 1) == "-") {
        is_negative = true
        s = s.substring(1, s.length)
    }
    if (s.length == 0) {
        panic("Non-octal value was passed to the function")
        return 0
    }
    var decimal_number: Int = 0
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        var digit: Int = char_to_digit(ch)
        decimal_number = (8 * decimal_number) + digit
        i = i + 1
    }
    if ((is_negative as Boolean)) {
        decimal_number = 0 - decimal_number
    }
    return decimal_number
}

fun user_main(): Unit {
    println(oct_to_decimal("1").toString())
    println(oct_to_decimal("-1").toString())
    println(oct_to_decimal("12").toString())
    println(oct_to_decimal(" 12   ").toString())
    println(oct_to_decimal("-45").toString())
    println(oct_to_decimal("0").toString())
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
