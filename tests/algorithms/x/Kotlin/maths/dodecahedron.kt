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

fun sqrtApprox(x: Double): Double {
    var guess: Double = x / 2.0
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun abs_val(num: Double): Double {
    if (num < 0.0) {
        return 0.0 - num
    }
    return num
}

fun approx_equal(a: Double, b: Double, eps: Double): Boolean {
    return abs_val(a - b) < eps
}

fun dodecahedron_surface_area(edge: Int): Double {
    if (edge <= 0) {
        panic("Length must be a positive.")
    }
    var term: Double = sqrtApprox(25.0 + (10.0 * sqrtApprox(5.0)))
    var e: Double = (edge.toDouble())
    return ((3.0 * term) * e) * e
}

fun dodecahedron_volume(edge: Int): Double {
    if (edge <= 0) {
        panic("Length must be a positive.")
    }
    var term: Double = (15.0 + (7.0 * sqrtApprox(5.0))) / 4.0
    var e: Double = (edge.toDouble())
    return ((term * e) * e) * e
}

fun test_dodecahedron(): Unit {
    if (!approx_equal(dodecahedron_surface_area(5), 516.1432201766901, 0.0001)) {
        panic("surface area 5 failed")
    }
    if (!approx_equal(dodecahedron_surface_area(10), 2064.5728807067603, 0.0001)) {
        panic("surface area 10 failed")
    }
    if (!approx_equal(dodecahedron_volume(5), 957.8898700780791, 0.0001)) {
        panic("volume 5 failed")
    }
    if (!approx_equal(dodecahedron_volume(10), 7663.118960624633, 0.0001)) {
        panic("volume 10 failed")
    }
}

fun user_main(): Unit {
    test_dodecahedron()
    println(dodecahedron_surface_area(5))
    println(dodecahedron_volume(5))
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
