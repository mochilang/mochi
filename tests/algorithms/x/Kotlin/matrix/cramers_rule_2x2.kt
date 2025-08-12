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

fun cramers_rule_2x2(eq1: MutableList<Double>, eq2: MutableList<Double>): MutableList<Double> {
    if ((eq1.size != 3) || (eq2.size != 3)) {
        panic("Please enter a valid equation.")
    }
    if ((((((eq1[0]!! == 0.0) && (eq1[1]!! == 0.0) as Boolean)) && (eq2[0]!! == 0.0) as Boolean)) && (eq2[1]!! == 0.0)) {
        panic("Both a & b of two equations can't be zero.")
    }
    var a1: Double = eq1[0]!!
    var b1: Double = eq1[1]!!
    var c1: Double = eq1[2]!!
    var a2: Double = eq2[0]!!
    var b2: Double = eq2[1]!!
    var c2: Double = eq2[2]!!
    var determinant: Double = (a1 * b2) - (a2 * b1)
    var determinant_x: Double = (c1 * b2) - (c2 * b1)
    var determinant_y: Double = (a1 * c2) - (a2 * c1)
    if (determinant == 0.0) {
        if ((determinant_x == 0.0) && (determinant_y == 0.0)) {
            panic("Infinite solutions. (Consistent system)")
        }
        panic("No solution. (Inconsistent system)")
    }
    if ((determinant_x == 0.0) && (determinant_y == 0.0)) {
        return mutableListOf(0.0, 0.0)
    }
    var x: Double = determinant_x / determinant
    var y: Double = determinant_y / determinant
    return mutableListOf(x, y)
}

fun test_cramers_rule_2x2(): Unit {
    var r1: MutableList<Double> = cramers_rule_2x2(mutableListOf(2.0, 3.0, 0.0), mutableListOf(5.0, 1.0, 0.0))
    if ((r1[0]!! != 0.0) || (r1[1]!! != 0.0)) {
        panic("Test1 failed")
    }
    var r2: MutableList<Double> = cramers_rule_2x2(mutableListOf(0.0, 4.0, 50.0), mutableListOf(2.0, 0.0, 26.0))
    if ((r2[0]!! != 13.0) || (r2[1]!! != 12.5)) {
        panic("Test2 failed")
    }
}

fun user_main(): Unit {
    test_cramers_rule_2x2()
    println(cramers_rule_2x2(mutableListOf(11.0, 2.0, 30.0), mutableListOf(1.0, 0.0, 4.0)))
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
