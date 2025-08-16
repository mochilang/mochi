import java.math.BigInteger

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

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

fun floor_div(a: Int, b: Int): Int {
    var q: Int = (a / b).toInt()
    var r: Int = (Math.floorMod(a, b)).toInt()
    if ((r != 0) && (((((a < 0) && (b > 0) as Boolean)) || (((a > 0) && (b < 0) as Boolean)) as Boolean))) {
        q = q - 1
    }
    return q
}

fun continued_fraction(numerator: Int, denominator: Int): MutableList<Int> {
    var num: Int = (numerator).toInt()
    var den: Int = (denominator).toInt()
    var result: MutableList<Int> = mutableListOf<Int>()
    while (true) {
        var integer_part: Int = (floor_div(num, den)).toInt()
        result = run { val _tmp = result.toMutableList(); _tmp.add(integer_part); _tmp }
        num = num - (integer_part * den)
        if (num == 0) {
            break
        }
        var tmp: Int = (num).toInt()
        num = den
        den = tmp
    }
    return result
}

fun list_to_string(lst: MutableList<Int>): String {
    var s: String = "["
    var i: Int = (0).toInt()
    while (i < lst.size) {
        s = s + _numToStr(lst[i]!!)
        if (i < (lst.size - 1)) {
            s = s + ", "
        }
        i = i + 1
    }
    return s + "]"
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Continued Fraction of 0.84375 is: " + list_to_string(continued_fraction(27, 32)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
