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

fun powf(base: Double, exponent: Int): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < exponent) {
        result = result * base
        i = i + 1
    }
    return result
}

fun round2(value: Double): Double {
    if (value >= 0.0) {
        var scaled: Int = ((((value * 100.0) + 0.5).toInt())).toInt()
        return ((scaled.toDouble())) / 100.0
    }
    var scaled: Int = ((((value * 100.0) - 0.5).toInt())).toInt()
    return ((scaled.toDouble())) / 100.0
}

fun present_value(discount_rate: Double, cash_flows: MutableList<Double>): Double {
    if (discount_rate < 0.0) {
        panic("Discount rate cannot be negative")
    }
    if (cash_flows.size == 0) {
        panic("Cash flows list cannot be empty")
    }
    var pv: Double = 0.0
    var i: Int = (0).toInt()
    var factor: Double = 1.0 + discount_rate
    while (i < cash_flows.size) {
        var cf: Double = cash_flows[i]!!
        pv = pv + (cf / powf(factor, i))
        i = i + 1
    }
    return round2(pv)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(present_value(0.13, mutableListOf(10.0, 20.7, 0.0 - 293.0, 297.0)).toString())
        println(present_value(0.07, mutableListOf(0.0 - 109129.39, 30923.23, 15098.93, 29734.0, 39.0)).toString())
        println(present_value(0.07, mutableListOf(109129.39, 30923.23, 15098.93, 29734.0, 39.0)).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
