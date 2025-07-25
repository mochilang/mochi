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

fun d2d(d: Double): Double {
    return d % 360.0
}

fun g2g(g: Double): Double {
    return g % 400.0
}

fun m2m(m: Double): Double {
    return m % 6400.0
}

fun r2r(r: Double): Double {
    return r % (2.0 * 3.141592653589793)
}

fun d2g(d: Double): Double {
    return (d2d(d) * 400.0) / 360.0
}

fun d2m(d: Double): Double {
    return (d2d(d) * 6400.0) / 360.0
}

fun d2r(d: Double): Double {
    return (d2d(d) * 3.141592653589793) / 180.0
}

fun g2d(g: Double): Double {
    return (g2g(g) * 360.0) / 400.0
}

fun g2m(g: Double): Double {
    return (g2g(g) * 6400.0) / 400.0
}

fun g2r(g: Double): Double {
    return (g2g(g) * 3.141592653589793) / 200.0
}

fun m2d(m: Double): Double {
    return (m2m(m) * 360.0) / 6400.0
}

fun m2g(m: Double): Double {
    return (m2m(m) * 400.0) / 6400.0
}

fun m2r(m: Double): Double {
    return (m2m(m) * 3.141592653589793) / 3200.0
}

fun r2d(r: Double): Double {
    return (r2r(r) * 180.0) / 3.141592653589793
}

fun r2g(r: Double): Double {
    return (r2r(r) * 200.0) / 3.141592653589793
}

fun r2m(r: Double): Double {
    return (r2r(r) * 3200.0) / 3.141592653589793
}

fun user_main(): Unit {
    val angles: MutableList<Double> = mutableListOf(0.0 - 2.0, 0.0 - 1.0, 0.0, 1.0, 2.0, 6.2831853, 16.0, 57.2957795, 359.0, 399.0, 6399.0, 1000000.0)
    println("degrees normalized_degs gradians mils radians")
    for (a in angles) {
        println((((((((a.toString() + " ") + d2d(a).toString()) + " ") + d2g(a).toString()) + " ") + d2m(a).toString()) + " ") + d2r(a).toString())
    }
    println("\ngradians normalized_grds degrees mils radians")
    for (a in angles) {
        println((((((((a.toString() + " ") + g2g(a).toString()) + " ") + g2d(a).toString()) + " ") + g2m(a).toString()) + " ") + g2r(a).toString())
    }
    println("\nmils normalized_mils degrees gradians radians")
    for (a in angles) {
        println((((((((a.toString() + " ") + m2m(a).toString()) + " ") + m2d(a).toString()) + " ") + m2g(a).toString()) + " ") + m2r(a).toString())
    }
    println("\nradians normalized_rads degrees gradians mils")
    for (a in angles) {
        println((((((((a.toString() + " ") + r2r(a).toString()) + " ") + r2d(a).toString()) + " ") + r2g(a).toString()) + " ") + r2m(a).toString())
    }
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
