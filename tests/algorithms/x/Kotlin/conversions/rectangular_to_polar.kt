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
fun sqrtApprox(x: Double): Double {
    var guess: Double = x / 2.0
    var i: Int = 0
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
        var r: Double = atanApprox(y / x)
        return r
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

fun deg(rad: Double): Double {
    return (rad * 180.0) / PI
}

fun floor(x: Double): Double {
    var i: Int = (x.toInt())
    if (((i.toDouble())) > x) {
        i = i - 1
    }
    return (i.toDouble())
}

fun pow10(n: Int): Double {
    var p: Double = 1.0
    var i: Int = 0
    while (i < n) {
        p = p * 10.0
        i = i + 1
    }
    return p
}

fun round(x: Double, n: Int): Double {
    var m: Double = pow10(n)
    return floor((x * m) + 0.5) / m
}

fun rectangular_to_polar(real: Double, img: Double): MutableList<Double> {
    var mod: Double = round(sqrtApprox((real * real) + (img * img)), 2)
    var ang: Double = round(deg(atan2Approx(img, real)), 2)
    return mutableListOf(mod, ang)
}

fun show(real: Double, img: Double): Unit {
    var r: MutableList<Double> = rectangular_to_polar(real, img)
    println(r.toString())
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        show(5.0, 0.0 - 5.0)
        show(0.0 - 1.0, 1.0)
        show(0.0 - 1.0, 0.0 - 1.0)
        show(0.0000000001, 0.0000000001)
        show(0.0 - 0.0000000001, 0.0000000001)
        show(9.75, 5.93)
        show(10000.0, 99999.0)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
