fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }

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

var PI: Double = 3.141592653589793
fun abs(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun to_radians(deg: Double): Double {
    return (deg * PI) / 180.0
}

fun sin_taylor(x: Double): Double {
    var term: Double = x
    var sum: Double = x
    var i: Int = (1).toInt()
    while (i < 10) {
        var k1: Double = 2.0 * ((i.toDouble()))
        var k2: Double = k1 + 1.0
        term = (((0.0 - term) * x) * x) / (k1 * k2)
        sum = sum + term
        i = i + 1
    }
    return sum
}

fun cos_taylor(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var i: Int = (1).toInt()
    while (i < 10) {
        var k1: Double = (2.0 * ((i.toDouble()))) - 1.0
        var k2: Double = 2.0 * ((i.toDouble()))
        term = (((0.0 - term) * x) * x) / (k1 * k2)
        sum = sum + term
        i = i + 1
    }
    return sum
}

fun rect(mag: Double, angle: Double): MutableList<Double> {
    var c: Double = cos_taylor(angle)
    var s: Double = sin_taylor(angle)
    return mutableListOf(mag * c, mag * s)
}

fun multiply(a: MutableList<Double>, b: MutableList<Double>): MutableList<Double> {
    return mutableListOf((a[0]!! * b[0]!!) - (a[1]!! * b[1]!!), (a[0]!! * b[1]!!) + (a[1]!! * b[0]!!))
}

fun apparent_power(voltage: Double, current: Double, voltage_angle: Double, current_angle: Double): MutableList<Double> {
    var vrad: Double = to_radians(voltage_angle)
    var irad: Double = to_radians(current_angle)
    var vrect: MutableList<Double> = rect(voltage, vrad)
    var irect: MutableList<Double> = rect(current, irad)
    var result: MutableList<Double> = multiply(vrect, irect)
    return result
}

fun approx_equal(a: MutableList<Double>, b: MutableList<Double>, eps: Double): Boolean {
    return (((abs(a[0]!! - b[0]!!) < eps) && (abs(a[1]!! - b[1]!!) < eps)) as Boolean)
}

fun test_zero_phase(): Unit {
    var s: MutableList<Double> = apparent_power(100.0, 5.0, 0.0, 0.0)
    var expected: MutableList<Double> = mutableListOf(500.0, 0.0)
    expect(approx_equal(s, expected, 0.001))
}

fun test_orthogonal_voltage(): Unit {
    var s: MutableList<Double> = apparent_power(100.0, 5.0, 90.0, 0.0)
    var expected: MutableList<Double> = mutableListOf(0.0, 500.0)
    expect(approx_equal(s, expected, 0.5))
}

fun test_negative_angles(): Unit {
    var s: MutableList<Double> = apparent_power(100.0, 5.0, 0.0 - 45.0, 0.0 - 60.0)
    var expected: MutableList<Double> = mutableListOf(0.0 - 129.40952255126027, 0.0 - 482.9629131445341)
    expect(approx_equal(s, expected, 0.001))
}

fun test_another_case(): Unit {
    var s: MutableList<Double> = apparent_power(200.0, 10.0, 0.0 - 30.0, 0.0 - 90.0)
    var expected: MutableList<Double> = mutableListOf(0.0 - 1000.0, 0.0 - 1732.0508075688776)
    expect(approx_equal(s, expected, 0.001))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        test_zero_phase()
        test_orthogonal_voltage()
        test_negative_angles()
        test_another_case()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
