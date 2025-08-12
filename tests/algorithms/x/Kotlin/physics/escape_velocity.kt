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

fun pow10(n: Int): Double {
    var p: Double = 1.0
    var k: Int = (0).toInt()
    if (n >= 0) {
        while (k < n) {
            p = p * 10.0
            k = k + 1
        }
    } else {
        var m: Int = (0 - n).toInt()
        while (k < m) {
            p = p / 10.0
            k = k + 1
        }
    }
    return p
}

fun sqrt_newton(n: Double): Double {
    if (n == 0.0) {
        return 0.0
    }
    var x: Double = n
    var j: Int = (0).toInt()
    while (j < 20) {
        x = (x + (n / x)) / 2.0
        j = j + 1
    }
    return x
}

fun round3(x: Double): Double {
    var y: Double = (x * 1000.0) + 0.5
    var yi: Int = ((y.toInt())).toInt()
    if (((yi.toDouble())) > y) {
        yi = yi - 1
    }
    return ((yi.toDouble())) / 1000.0
}

fun escape_velocity(mass: Double, radius: Double): Double {
    if (radius == 0.0) {
        panic("Radius cannot be zero.")
    }
    var G: Double = 6.6743 * pow10(0 - 11)
    var velocity: Double = sqrt_newton(((2.0 * G) * mass) / radius)
    return round3(velocity)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(escape_velocity(5.972 * pow10(24), 6.371 * pow10(6)))
        println(escape_velocity(7.348 * pow10(22), 1.737 * pow10(6)))
        println(escape_velocity(1.898 * pow10(27), 6.9911 * pow10(7)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
