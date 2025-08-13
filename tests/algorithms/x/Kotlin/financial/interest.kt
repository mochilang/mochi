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

fun powf(base: Double, exp: Double): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < ((exp.toInt()))) {
        result = result * base
        i = i + 1
    }
    return result
}

fun simple_interest(principal: Double, daily_rate: Double, days: Double): Double {
    if (days <= 0.0) {
        panic("days_between_payments must be > 0")
        return 0.0
    }
    if (daily_rate < 0.0) {
        panic("daily_interest_rate must be >= 0")
        return 0.0
    }
    if (principal <= 0.0) {
        panic("principal must be > 0")
        return 0.0
    }
    return (principal * daily_rate) * days
}

fun compound_interest(principal: Double, nominal_rate: Double, periods: Double): Double {
    if (periods <= 0.0) {
        panic("number_of_compounding_periods must be > 0")
        return 0.0
    }
    if (nominal_rate < 0.0) {
        panic("nominal_annual_interest_rate_percentage must be >= 0")
        return 0.0
    }
    if (principal <= 0.0) {
        panic("principal must be > 0")
        return 0.0
    }
    return principal * (powf(1.0 + nominal_rate, periods) - 1.0)
}

fun apr_interest(principal: Double, apr: Double, years: Double): Double {
    if (years <= 0.0) {
        panic("number_of_years must be > 0")
        return 0.0
    }
    if (apr < 0.0) {
        panic("nominal_annual_percentage_rate must be >= 0")
        return 0.0
    }
    if (principal <= 0.0) {
        panic("principal must be > 0")
        return 0.0
    }
    return compound_interest(principal, apr / 365.0, years * 365.0)
}

fun user_main(): Unit {
    println(simple_interest(18000.0, 0.06, 3.0).toString())
    println(simple_interest(0.5, 0.06, 3.0).toString())
    println(simple_interest(18000.0, 0.01, 10.0).toString())
    println(compound_interest(10000.0, 0.05, 3.0).toString())
    println(compound_interest(10000.0, 0.05, 1.0).toString())
    println(apr_interest(10000.0, 0.05, 3.0).toString())
    println(apr_interest(10000.0, 0.05, 1.0).toString())
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
