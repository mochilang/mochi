fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }

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

var roman_values: MutableList<Int> = mutableListOf(1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1)
var roman_symbols: MutableList<String> = mutableListOf("M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I")
fun char_value(c: String): Int {
    if (c == "I") {
        return 1
    }
    if (c == "V") {
        return 5
    }
    if (c == "X") {
        return 10
    }
    if (c == "L") {
        return 50
    }
    if (c == "C") {
        return 100
    }
    if (c == "D") {
        return 500
    }
    if (c == "M") {
        return 1000
    }
    return 0
}

fun roman_to_int(roman: String): Int {
    var total: Int = 0
    var i: Int = 0
    while (i < roman.length) {
        if (((i + 1) < roman.length) && (char_value(roman[i].toString()) < char_value(roman[i + 1].toString()))) {
            total = (total + char_value(roman[i + 1].toString())) - char_value(roman[i].toString())
            i = i + 2
        } else {
            total = total + char_value(roman[i].toString())
            i = i + 1
        }
    }
    return total
}

fun int_to_roman(number: Int): String {
    var num: Int = number
    var res: String = ""
    var i: Int = 0
    while (i < roman_values.size) {
        var value: Int = roman_values[i]!!
        var symbol: String = roman_symbols[i]!!
        var factor: Int = num / value
        num = Math.floorMod(num, value)
        var j: Int = 0
        while (j < factor) {
            res = res + symbol
            j = j + 1
        }
        if (num == 0) {
            break
        }
        i = i + 1
    }
    return res
}

fun test_roman_to_int(): Unit {
    expect(roman_to_int("III") == 3)
    expect(roman_to_int("CLIV") == 154)
    expect(roman_to_int("MIX") == 1009)
    expect(roman_to_int("MMD") == 2500)
    expect(roman_to_int("MMMCMXCIX") == 3999)
}

fun test_int_to_roman(): Unit {
    expect(int_to_roman(3) == "III")
    expect(int_to_roman(154) == "CLIV")
    expect(int_to_roman(1009) == "MIX")
    expect(int_to_roman(2500) == "MMD")
    expect(int_to_roman(3999) == "MMMCMXCIX")
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        test_roman_to_int()
        test_int_to_roman()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
