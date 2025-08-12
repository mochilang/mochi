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

fun centripetal(mass: Double, velocity: Double, radius: Double): Double {
    if (mass < 0.0) {
        panic("The mass of the body cannot be negative")
    }
    if (radius <= 0.0) {
        panic("The radius is always a positive non zero integer")
    }
    return ((mass * velocity) * velocity) / radius
}

fun floor(x: Double): Double {
    var i: Int = ((x.toInt())).toInt()
    if (((i.toDouble())) > x) {
        i = i - 1
    }
    return (i.toDouble())
}

fun pow10(n: Int): Double {
    var p: Double = 1.0
    var i: Int = (0).toInt()
    while (i < n) {
        p = p * 10.0
        i = i + 1
    }
    return p
}

fun round(x: Double, n: Int): Double {
    var m: Double = pow10(n)
    return kotlin.math.floor((x * m) + 0.5) / m
}

fun show(mass: Double, velocity: Double, radius: Double): Unit {
    var f: Double = centripetal(mass, velocity, radius)
    println(round(f, 2).toString())
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        show(15.5, 0.0 - 30.0, 10.0)
        show(10.0, 15.0, 5.0)
        show(20.0, 0.0 - 50.0, 15.0)
        show(12.25, 40.0, 25.0)
        show(50.0, 100.0, 50.0)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
