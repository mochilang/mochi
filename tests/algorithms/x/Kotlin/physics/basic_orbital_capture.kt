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

var G: Double = 0.000000000066743
var C: Double = 299792458.0
var PI: Double = 3.141592653589793
fun pow10(n: Int): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < n) {
        result = result * 10.0
        i = i + 1
    }
    return result
}

fun sqrt(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun abs(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun capture_radii(target_body_radius: Double, target_body_mass: Double, projectile_velocity: Double): Double {
    if (target_body_mass < 0.0) {
        panic("Mass cannot be less than 0")
    }
    if (target_body_radius < 0.0) {
        panic("Radius cannot be less than 0")
    }
    if (projectile_velocity > C) {
        panic("Cannot go beyond speed of light")
    }
    var escape_velocity_squared: Double = ((2.0 * G) * target_body_mass) / target_body_radius
    var denom: Double = projectile_velocity * projectile_velocity
    var capture_radius: Double = target_body_radius * sqrt(1.0 + (escape_velocity_squared / denom))
    return capture_radius
}

fun capture_area(capture_radius: Double): Double {
    if (capture_radius < 0.0) {
        panic("Cannot have a capture radius less than 0")
    }
    var sigma: Double = (PI * capture_radius) * capture_radius
    return sigma
}

fun run_tests(): Unit {
    var r: Double = capture_radii(6.957 * pow10(8), 1.99 * pow10(30), 25000.0)
    if (kotlin.math.abs(r - (1.720959069143714 * pow10(10))) > 1.0) {
        panic("capture_radii failed")
    }
    var a: Double = capture_area(r)
    if (kotlin.math.abs(a - (9.304455331801812 * pow10(20))) > 1.0) {
        panic("capture_area failed")
    }
}

fun user_main(): Unit {
    run_tests()
    var r: Double = capture_radii(6.957 * pow10(8), 1.99 * pow10(30), 25000.0)
    println(r.toString())
    println(capture_area(r).toString())
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
