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

var ones: MutableList<String> = mutableListOf("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
var teens: MutableList<String> = mutableListOf("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
var tens: MutableList<String> = mutableListOf("", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
var short_powers: MutableList<Int> = mutableListOf(15, 12, 9, 6, 3, 2)
var short_units: MutableList<String> = mutableListOf("quadrillion", "trillion", "billion", "million", "thousand", "hundred")
var long_powers: MutableList<Int> = mutableListOf(15, 9, 6, 3, 2)
var long_units: MutableList<String> = mutableListOf("billiard", "milliard", "million", "thousand", "hundred")
var indian_powers: MutableList<Int> = mutableListOf(14, 12, 7, 5, 3, 2)
var indian_units: MutableList<String> = mutableListOf("crore crore", "lakh crore", "crore", "lakh", "thousand", "hundred")
fun pow10(exp: Int): Int {
    var res: Int = 1
    var i: Int = 0
    while (i < exp) {
        res = res * 10
        i = i + 1
    }
    return res
}

fun max_value(system: String): Int {
    if (system == "short") {
        return pow10(18) - 1
    }
    if (system == "long") {
        return pow10(21) - 1
    }
    if (system == "indian") {
        return pow10(19) - 1
    }
    return 0
}

fun join_words(words: MutableList<String>): String {
    var res: String = ""
    var i: Int = 0
    while (i < words.size) {
        if (i > 0) {
            res = res + " "
        }
        res = res + words[i]!!
        i = i + 1
    }
    return res
}

fun convert_small_number(num: Int): String {
    if (num < 0) {
        return ""
    }
    if (num >= 100) {
        return ""
    }
    var tens_digit: Int = num / 10
    var ones_digit: Int = Math.floorMod(num, 10)
    if (tens_digit == 0) {
        return ones[ones_digit]!!
    }
    if (tens_digit == 1) {
        return teens[ones_digit]!!
    }
    var hyphen: String = (if (ones_digit > 0) "-" else "" as String)
    var tail: String = (if (ones_digit > 0) ones[ones_digit]!! else "" as String)
    return (tens[tens_digit]!! + hyphen) + tail
}

fun convert_number(num: Int, system: String): String {
    var word_groups: MutableList<String> = mutableListOf<String>()
    var n: Int = num
    if (n < 0) {
        word_groups = run { val _tmp = word_groups.toMutableList(); _tmp.add("negative"); _tmp }
        n = 0 - n
    }
    if (n > max_value(system)) {
        return ""
    }
    var powers: MutableList<Int> = mutableListOf<Int>()
    var units: MutableList<String> = mutableListOf<String>()
    if (system == "short") {
        powers = short_powers
        units = short_units
    } else {
        if (system == "long") {
            powers = long_powers
            units = long_units
        } else {
            if (system == "indian") {
                powers = indian_powers
                units = indian_units
            } else {
                return ""
            }
        }
    }
    var i: Int = 0
    while (i < powers.size) {
        var power: Int = powers[i]!!
        var unit: String = units[i]!!
        var divisor: Int = pow10(power)
        var digit_group: Int = n / divisor
        n = Math.floorMod(n, divisor)
        if (digit_group > 0) {
            var word_group: String = (if (digit_group >= 100) convert_number(digit_group, system) else convert_small_number(digit_group) as String)
            word_groups = run { val _tmp = word_groups.toMutableList(); _tmp.add((word_group + " ") + unit); _tmp }
        }
        i = i + 1
    }
    if ((n > 0) || (word_groups.size == 0)) {
        word_groups = run { val _tmp = word_groups.toMutableList(); _tmp.add(convert_small_number((n.toInt()))); _tmp }
    }
    var joined: String = join_words(word_groups)
    return joined
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(convert_number((123456789012345L.toInt()), "short"))
        println(convert_number((123456789012345L.toInt()), "long"))
        println(convert_number((123456789012345L.toInt()), "indian"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
