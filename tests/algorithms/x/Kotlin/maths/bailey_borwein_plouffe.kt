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

var digits: String = ""
var i: Int = (1).toInt()
fun mod_pow(base: Int, exponent: Int, modulus: Int): Int {
    var result: Int = (1).toInt()
    var b: BigInteger = ((Math.floorMod(base, modulus)).toBigInteger())
    var e: Int = (exponent).toInt()
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            result = ((((result).toBigInteger().multiply((b))).mod((modulus).toBigInteger())).toInt())
        }
        b = (b.multiply((b))).mod((modulus).toBigInteger())
        e = e / 2
    }
    return result
}

fun pow_float(base: Double, exponent: Int): Double {
    var exp: Int = (exponent).toInt()
    var result: Double = 1.0
    if (exp < 0) {
        exp = 0 - exp
    }
    var i: Int = (0).toInt()
    while (i < exp) {
        result = result * base
        i = (i + 1).toInt()
    }
    if (exponent < 0) {
        result = 1.0 / result
    }
    return result
}

fun hex_digit(n: Int): String {
    if (n < 10) {
        return n.toString()
    }
    var letters: MutableList<String> = mutableListOf("a", "b", "c", "d", "e", "f")
    return letters[n - 10]!!
}

fun floor_float(x: Double): Double {
    var i: Int = ((x.toInt())).toInt()
    if (((i.toDouble())) > x) {
        i = (i - 1).toInt()
    }
    return (i.toDouble())
}

fun subsum(digit_pos_to_extract: Int, denominator_addend: Int, precision: Int): Double {
    var total: Double = 0.0
    var sum_index: Int = (0).toInt()
    while (sum_index < (digit_pos_to_extract + precision)) {
        var denominator: Int = ((8 * sum_index) + denominator_addend).toInt()
        if (sum_index < digit_pos_to_extract) {
            var exponent: Int = ((digit_pos_to_extract - 1) - sum_index).toInt()
            var exponential_term: Int = (mod_pow(16, exponent, denominator)).toInt()
            total = total + (((exponential_term.toDouble())) / ((denominator.toDouble())))
        } else {
            var exponent: Int = ((digit_pos_to_extract - 1) - sum_index).toInt()
            var exponential_term: Int = (pow_float(16.0, exponent)).toInt()
            total = total + (exponential_term / ((denominator.toDouble())))
        }
        sum_index = sum_index + 1
    }
    return total
}

fun bailey_borwein_plouffe(digit_position: Int, precision: Int): String {
    if (digit_position <= 0) {
        panic("Digit position must be a positive integer")
    }
    if (precision < 0) {
        panic("Precision must be a nonnegative integer")
    }
    var sum_result: Double = (((4.0 * subsum(digit_position, 1, precision)) - (2.0 * subsum(digit_position, 4, precision))) - (1.0 * subsum(digit_position, 5, precision))) - (1.0 * subsum(digit_position, 6, precision))
    var fraction: Double = sum_result - floor_float(sum_result)
    var digit: Int = (((fraction * 16.0).toInt())).toInt()
    var hd: String = hex_digit(digit)
    return hd
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (i <= 10) {
            digits = digits + bailey_borwein_plouffe(i, 1000)
            i = (i + 1).toInt()
        }
        println(digits)
        println(bailey_borwein_plouffe(5, 10000))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
