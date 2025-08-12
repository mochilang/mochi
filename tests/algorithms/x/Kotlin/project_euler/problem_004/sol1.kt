import java.math.BigInteger

fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
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

fun is_palindrome(num: Int): Boolean {
    var s: String = num.toString()
    var i: Int = (0).toInt()
    var j: BigInteger = ((s.length - 1).toBigInteger())
    while ((i).toBigInteger().compareTo((j)) < 0) {
        if (_sliceStr(s, i, i + 1) != _sliceStr(s, (j).toInt(), (j.add((1).toBigInteger())).toInt())) {
            return false
        }
        i = i + 1
        j = j.subtract((1).toBigInteger())
    }
    return true
}

fun solution(n: Int): Int {
    var number: BigInteger = ((n - 1).toBigInteger())
    while (number.compareTo((9999).toBigInteger()) > 0) {
        if (((is_palindrome((number.toInt()))) as Boolean)) {
            var divisor: Int = (999).toInt()
            while (divisor > 99) {
                if ((number.mod((divisor).toBigInteger())).compareTo((0).toBigInteger()) == 0) {
                    var other = number.divide((divisor).toBigInteger())
                    if (other.toString().length == 3) {
                        return (number.toInt())
                    }
                }
                divisor = divisor - 1
            }
        }
        number = number.subtract((1).toBigInteger())
    }
    println("That number is larger than our acceptable range.")
    return 0
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("solution() = " + solution(998001).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
