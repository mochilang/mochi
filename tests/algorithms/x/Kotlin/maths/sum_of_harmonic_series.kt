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

fun sum_of_harmonic_progression(first_term: Double, common_difference: Double, number_of_terms: Int): Double {
    var arithmetic_progression: MutableList<Double> = mutableListOf(1.0 / first_term)
    var term: Double = 1.0 / first_term
    var i: Int = (0).toInt()
    while (i < (number_of_terms - 1)) {
        term = term + common_difference
        arithmetic_progression = run { val _tmp = arithmetic_progression.toMutableList(); _tmp.add(term); _tmp }
        i = i + 1
    }
    var total: Double = 0.0
    var j: Int = (0).toInt()
    while (j < arithmetic_progression.size) {
        total = total + (1.0 / arithmetic_progression[j]!!)
        j = j + 1
    }
    return total
}

fun abs_val(num: Double): Double {
    if (num < 0.0) {
        return 0.0 - num
    }
    return num
}

fun test_sum_of_harmonic_progression(): Unit {
    var result1: Double = sum_of_harmonic_progression(0.5, 2.0, 2)
    if (abs_val(result1 - 0.75) > 0.0000001) {
        panic("test1 failed")
    }
    var result2: Double = sum_of_harmonic_progression(0.2, 5.0, 5)
    if (abs_val(result2 - 0.45666666666666667) > 0.0000001) {
        panic("test2 failed")
    }
}

fun user_main(): Unit {
    test_sum_of_harmonic_progression()
    println(sum_of_harmonic_progression(0.5, 2.0, 2))
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
