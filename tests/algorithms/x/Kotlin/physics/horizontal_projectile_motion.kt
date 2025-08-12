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

var PI: Double = 3.141592653589793
var TWO_PI: Double = 6.283185307179586
var g: Double = 9.80665
var v0: Double = 25.0
var angle: Double = 20.0
fun _mod(x: Double, m: Double): Double {
    return x - ((((((x / m).toInt())).toDouble())) * m)
}

fun sin(x: Double): Double {
    var y: Double = _mod(x + PI, TWO_PI) - PI
    var y2: Double = y * y
    var y3: Double = y2 * y
    var y5: Double = y3 * y2
    var y7: Double = y5 * y2
    return ((y - (y3 / 6.0)) + (y5 / 120.0)) - (y7 / 5040.0)
}

fun deg_to_rad(deg: Double): Double {
    return (deg * PI) / 180.0
}

fun floor(x: Double): Double {
    var i: Int = ((x.toInt())).toInt()
    if (((i.toDouble())) > x) {
        i = i - 1
    }
    return (i.toDouble())
}

fun pow10(n: Int): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < n) {
        result = result * 10.0
        i = i + 1
    }
    return result
}

fun round(x: Double, n: Int): Double {
    var m: Double = pow10(n)
    var y: Double = ((kotlin.math.floor((x * m) + 0.5)).toDouble())
    return y / m
}

fun check_args(init_velocity: Double, angle: Double): Unit {
    if ((angle > 90.0) || (angle < 1.0)) {
        panic("Invalid angle. Range is 1-90 degrees.")
    }
    if (init_velocity < 0.0) {
        panic("Invalid velocity. Should be a positive number.")
    }
}

fun horizontal_distance(init_velocity: Double, angle: Double): Double {
    check_args(init_velocity, angle)
    var radians: Double = deg_to_rad(2.0 * angle)
    return round(((init_velocity * init_velocity) * sin(radians)) / g, 2)
}

fun max_height(init_velocity: Double, angle: Double): Double {
    check_args(init_velocity, angle)
    var radians: Double = deg_to_rad(angle)
    var s: Double = sin(radians)
    return round((((init_velocity * init_velocity) * s) * s) / (2.0 * g), 2)
}

fun total_time(init_velocity: Double, angle: Double): Double {
    check_args(init_velocity, angle)
    var radians: Double = deg_to_rad(angle)
    return round(((2.0 * init_velocity) * sin(radians)) / g, 2)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(horizontal_distance(v0, angle))
        println(max_height(v0, angle))
        println(total_time(v0, angle))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
