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
var AXIS_A: Double = 6378137.0
var AXIS_B: Double = 6356752.314245
var RADIUS: Double = 6378137.0
var SAN_FRANCISCO: MutableList<Double> = mutableListOf(37.774856, 0.0 - 122.424227)
var YOSEMITE: MutableList<Double> = mutableListOf(37.864742, 0.0 - 119.537521)
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

fun tan_approx(x: Double): Double {
    return sin_taylor(x) / cos_taylor(x)
}

fun sqrtApprox(x: Double): Double {
    var guess: Double = x / 2.0
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun atanApprox(x: Double): Double {
    if (x > 1.0) {
        return (PI / 2.0) - (x / ((x * x) + 0.28))
    }
    if (x < (0.0 - 1.0)) {
        return ((0.0 - PI) / 2.0) - (x / ((x * x) + 0.28))
    }
    return x / (1.0 + ((0.28 * x) * x))
}

fun atan2Approx(y: Double, x: Double): Double {
    if (x > 0.0) {
        var _val: Double = atanApprox(y / x)
        return _val
    }
    if (x < 0.0) {
        if (y >= 0.0) {
            return atanApprox(y / x) + PI
        }
        return atanApprox(y / x) - PI
    }
    if (y > 0.0) {
        return PI / 2.0
    }
    if (y < 0.0) {
        return (0.0 - PI) / 2.0
    }
    return 0.0
}

fun asinApprox(x: Double): Double {
    var denom: Double = sqrtApprox(1.0 - (x * x))
    var res: Double = atan2Approx(x, denom)
    return res
}

fun haversine_distance(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double {
    var flattening: Double = (AXIS_A - AXIS_B) / AXIS_A
    var phi_1: Double = atanApprox((1.0 - flattening) * tan_approx(to_radians(lat1)))
    var phi_2: Double = atanApprox((1.0 - flattening) * tan_approx(to_radians(lat2)))
    var lambda_1: Double = to_radians(lon1)
    var lambda_2: Double = to_radians(lon2)
    var sin_sq_phi: Double = sin_taylor((phi_2 - phi_1) / 2.0)
    var sin_sq_lambda: Double = sin_taylor((lambda_2 - lambda_1) / 2.0)
    sin_sq_phi = sin_sq_phi * sin_sq_phi
    sin_sq_lambda = sin_sq_lambda * sin_sq_lambda
    var h_value: Double = sqrtApprox(sin_sq_phi + ((cos_taylor(phi_1) * cos_taylor(phi_2)) * sin_sq_lambda))
    return (2.0 * RADIUS) * asinApprox(h_value)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(haversine_distance(SAN_FRANCISCO[0]!!, SAN_FRANCISCO[1]!!, YOSEMITE[0]!!, YOSEMITE[1]!!).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
