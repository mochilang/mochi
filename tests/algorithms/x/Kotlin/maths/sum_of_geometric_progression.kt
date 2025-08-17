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

fun pow_float(base: Double, exp: Int): Double {
    var result: Double = 1.0
    var exponent: Int = (exp).toInt()
    if (exponent < 0) {
        exponent = 0 - exponent
        var i: Int = (0).toInt()
        while (i < exponent) {
            result = result * base
            i = i + 1
        }
        return 1.0 / result
    }
    var i: Int = (0).toInt()
    while (i < exponent) {
        result = result * base
        i = i + 1
    }
    return result
}

fun sum_of_geometric_progression(first_term: Int, common_ratio: Int, num_of_terms: Int): Double {
    if (common_ratio == 1) {
        return (num_of_terms * first_term).toDouble()
    }
    var a: Double = first_term.toDouble()
    var r: Double = common_ratio.toDouble()
    return (a / (1.0 - r)) * (1.0 - pow_float(r, num_of_terms))
}

fun test_sum(): Unit {
    if (sum_of_geometric_progression(1, 2, 10) != 1023.0) {
        panic("example1 failed")
    }
    if (sum_of_geometric_progression(1, 10, 5) != 11111.0) {
        panic("example2 failed")
    }
    if (sum_of_geometric_progression(0 - 1, 2, 10) != (0.0 - 1023.0)) {
        panic("example3 failed")
    }
}

fun user_main(): Unit {
    test_sum()
    println(sum_of_geometric_progression(1, 2, 10))
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
