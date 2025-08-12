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

fun pow(base: Double, exp: Int): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun sqrt_approx(x: Double): Double {
    if (x == 0.0) {
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

fun hubble_parameter(hubble_constant: Double, radiation_density: Double, matter_density: Double, dark_energy: Double, redshift: Double): Double {
    var parameters: MutableList<Double> = mutableListOf(redshift, radiation_density, matter_density, dark_energy)
    var i: Int = (0).toInt()
    while (i < parameters.size) {
        if (parameters[i]!! < 0.0) {
            panic("All input parameters must be positive")
        }
        i = i + 1
    }
    i = 1
    while (i < 4) {
        if (parameters[i]!! > 1.0) {
            panic("Relative densities cannot be greater than one")
        }
        i = i + 1
    }
    var curvature: Double = 1.0 - ((matter_density + radiation_density) + dark_energy)
    var zp1: Double = redshift + 1.0
    var e2: Double = (((radiation_density * pow(zp1, 4)) + (matter_density * pow(zp1, 3))) + (curvature * pow(zp1, 2))) + dark_energy
    return hubble_constant * sqrt_approx(e2)
}

fun test_hubble_parameter(): Unit {
    var h: Double = hubble_parameter(68.3, 0.0001, 0.3, 0.7, 0.0)
    if ((h < 68.2999) || (h > 68.3001)) {
        panic("hubble_parameter test failed")
    }
}

fun user_main(): Unit {
    test_hubble_parameter()
    println(hubble_parameter(68.3, 0.0001, 0.3, 0.7, 0.0))
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
