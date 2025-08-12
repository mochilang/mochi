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

fun sum_of_multiples(n: Int): Int {
    var total: Int = (0).toInt()
    var terms: BigInteger = (((n - 1) / 3).toBigInteger())
    total = (((total).toBigInteger().add(((terms.multiply(((6).toBigInteger().add(((terms.subtract((1).toBigInteger())).multiply((3).toBigInteger())))))).divide((2).toBigInteger())))).toInt())
    terms = (((n - 1) / 5).toBigInteger())
    total = (((total).toBigInteger().add(((terms.multiply(((10).toBigInteger().add(((terms.subtract((1).toBigInteger())).multiply((5).toBigInteger())))))).divide((2).toBigInteger())))).toInt())
    terms = (((n - 1) / 15).toBigInteger())
    total = (((total).toBigInteger().subtract(((terms.multiply(((30).toBigInteger().add(((terms.subtract((1).toBigInteger())).multiply((15).toBigInteger())))))).divide((2).toBigInteger())))).toInt())
    return total
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("solution() = " + sum_of_multiples(1000).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
