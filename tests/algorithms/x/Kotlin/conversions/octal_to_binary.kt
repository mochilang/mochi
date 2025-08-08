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

fun octal_to_binary(octal_number: String): String {
    if (octal_number.length == 0) {
        panic("Empty string was passed to the function")
    }
    var octal_digits: String = "01234567"
    var binary_number: String = ""
    var i: Int = 0
    while (i < octal_number.length) {
        var digit: String = octal_number[i].toString()
        var valid: Boolean = false
        var j: Int = 0
        while (j < octal_digits.length) {
            if (digit == octal_digits[j].toString()) {
                valid = true
                break
            }
            j = j + 1
        }
        if (!valid) {
            panic("Non-octal value was passed to the function")
        }
        var value: Int = (digit.toInt())
        var k: Int = 0
        var binary_digit: String = ""
        while (k < 3) {
            binary_digit = (Math.floorMod(value, 2)).toString() + binary_digit
            value = value / 2
            k = k + 1
        }
        binary_number = binary_number + binary_digit
        i = i + 1
    }
    return binary_number
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(octal_to_binary("17"))
        println(octal_to_binary("7"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
