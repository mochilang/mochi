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

data class NRResult(var root: Double = 0.0, var error: Double = 0.0, var steps: MutableList<Double> = mutableListOf<Double>())
var result: NRResult = newton_raphson(::poly, 0.4, 20, 0.000001, 0.000001, false)
fun abs_float(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    } else {
        return x
    }
}

fun fail(msg: String): Unit {
    println("error: " + msg)
}

fun calc_derivative(f: (Double) -> Double, x: Double, delta_x: Double): Double {
    return ((f(x + (delta_x / 2.0)) - f(x - (delta_x / 2.0))).toDouble()) / delta_x
}

fun newton_raphson(f: (Double) -> Double, x0: Double, max_iter: Int, step: Double, max_error: Double, log_steps: Boolean): NRResult {
    var a: Double = x0
    var steps: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < max_iter) {
        if (log_steps as Boolean) {
            steps = run { val _tmp = steps.toMutableList(); _tmp.add(a); _tmp }
        }
        var err: Double = abs_float(f(a))
        if (err < max_error) {
            return NRResult(root = a, error = err, steps = steps)
        }
        var der: Double = calc_derivative(f, a, step)
        if (der == 0.0) {
            fail("No converging solution found, zero derivative")
            return NRResult(root = a, error = err, steps = steps)
        }
        a = a - (f(a) / der)
        i = i + 1
    }
    fail("No converging solution found, iteration limit reached")
    return NRResult(root = a, error = abs_float(f(a)), steps = steps)
}

fun poly(x: Double): Double {
    return (((((x * x).toDouble()) - (5.0 * x)).toDouble()) + 2.0).toDouble()
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println((("root = " + _numToStr(result.root)) + ", error = ") + _numToStr(result.error))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
