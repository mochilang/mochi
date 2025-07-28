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

data class DivResult(var q: Int, var r: Int)
fun egyptianDivide(dividend: Int, divisor: Int): DivResult {
    if ((dividend < 0) || (divisor <= 0)) {
        panic("Invalid argument(s)")
    }
    if (dividend < divisor) {
        return DivResult(q = 0, r = dividend)
    }
    var powers: MutableList<Int> = mutableListOf(1)
    var doublings: MutableList<Int> = mutableListOf(divisor)
    var doubling: BigInteger = (divisor * 2).toBigInteger()
    while (doubling.compareTo(dividend.toBigInteger()) <= 0) {
        powers = run { val _tmp = powers.toMutableList(); _tmp.add(powers[powers.size - 1] * 2); _tmp } as MutableList<Int>
        doublings = run { val _tmp = doublings.toMutableList(); _tmp.add(doubling.toInt()); _tmp } as MutableList<Int>
        doubling = doubling.multiply(2.toBigInteger())
    }
    var ans: Int = 0
    var accum: Int = 0
    var i: BigInteger = (doublings.size - 1).toBigInteger()
    while (i.compareTo(0.toBigInteger()) >= 0) {
        if ((accum + doublings[(i).toInt()]) <= dividend) {
            accum = accum + doublings[(i).toInt()]
            ans = ans + powers[(i).toInt()]
            if (accum == dividend) {
                break
            }
        }
        i = i.subtract(1.toBigInteger())
    }
    return DivResult(q = ans, r = dividend - accum)
}

fun user_main(): Unit {
    val dividend: Int = 580
    val divisor: Int = 34
    val res: DivResult = egyptianDivide(dividend, divisor)
    println((((((dividend.toString() + " divided by ") + divisor.toString()) + " is ") + res.q.toString()) + " with remainder ") + res.r.toString())
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
