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
var EQUATORIAL_RADIUS: Double = 6378137.0
fun to_radians(deg: Double): Double {
    return (deg * PI) / 180.0
}

fun sin_approx(x: Double): Double {
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

fun cos_approx(x: Double): Double {
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

fun sqrt_approx(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x / 2.0
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun lamberts_ellipsoidal_distance(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double {
    var phi1: Double = to_radians(lat1)
    var phi2: Double = to_radians(lat2)
    var lambda1: Double = to_radians(lon1)
    var lambda2: Double = to_radians(lon2)
    var x: Double = (lambda2 - lambda1) * cos_approx((phi1 + phi2) / 2.0)
    var y: Double = phi2 - phi1
    return EQUATORIAL_RADIUS * sqrt_approx((x * x) + (y * y))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(lamberts_ellipsoidal_distance(37.774856, 0.0 - 122.424227, 37.864742, 0.0 - 119.537521))
        println(lamberts_ellipsoidal_distance(37.774856, 0.0 - 122.424227, 40.713019, 0.0 - 74.012647))
        println(lamberts_ellipsoidal_distance(37.774856, 0.0 - 122.424227, 45.443012, 12.313071))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
