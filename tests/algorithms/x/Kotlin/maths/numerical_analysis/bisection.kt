fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

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

fun abs_float(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    } else {
        return x
    }
}

fun bisection(f: (Double) -> Double, a: Double, b: Double): Double {
    var start: Double = a
    var end: Double = b
    if (f(a) == 0.0) {
        return a
    }
    if (f(b) == 0.0) {
        return b
    }
    if ((f(a) * f(b)) > 0.0) {
        panic("could not find root in given interval.")
    }
    var mid: Double = start + ((end - start) / 2.0)
    while (abs_float(start - mid) > 0.0000001) {
        var fmid: Double = f(mid)
        if (fmid == 0.0) {
            return mid
        }
        if ((fmid * f(start)) < 0.0) {
            end = mid
        } else {
            start = mid
        }
        mid = start + ((end - start) / 2.0)
    }
    return mid
}

fun f(x: Double): Double {
    return (((((((x * x).toDouble()) * x).toDouble()) - (2.0 * x)).toDouble()) - 5.0).toDouble()
}

fun user_main(): Unit {
    println(_numToStr(bisection(::f, 1.0, 1000.0)))
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
