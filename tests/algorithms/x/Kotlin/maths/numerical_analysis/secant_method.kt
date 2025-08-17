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

fun exp_approx(x: Double): Double {
    var sum: Double = 1.0
    var term: Double = 1.0
    var i: Int = (1).toInt()
    while (i <= 20) {
        term = (term * x) / (i).toDouble()
        sum = sum + term
        i = i + 1
    }
    return sum
}

fun f(x: Double): Double {
    return (8.0 * x) - (2.0 * exp_approx(0.0 - x))
}

fun secant_method(lower_bound: Double, upper_bound: Double, repeats: Int): Double {
    var x0: Double = lower_bound
    var x1: Double = upper_bound
    var i: Int = (0).toInt()
    while (i < repeats) {
        var fx1: Double = f(x1)
        var fx0: Double = f(x0)
        var new_x: Double = x1 - ((fx1 * (x1 - x0)) / (fx1 - fx0))
        x0 = x1
        x1 = new_x
        i = i + 1
    }
    return x1
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(secant_method(1.0, 3.0, 2)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
