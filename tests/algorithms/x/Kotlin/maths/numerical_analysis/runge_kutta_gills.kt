fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

var y1: MutableList<Double> = runge_kutta_gills(::f1, 0.0, 3.0, 0.2, 5.0)
fun sqrt(x: Double): Double {
    var guess: Double = if (x > 1.0) x / 2.0 else 1.0.toDouble()
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = 0.5 * (guess + (x / guess))
        i = i + 1
    }
    return guess
}

fun runge_kutta_gills(func: (Double, Double) -> Double, x_initial: Double, y_initial: Double, step_size: Double, x_final: Double): MutableList<Double> {
    if (x_initial >= x_final) {
        panic("The final value of x must be greater than initial value of x.")
    }
    if (step_size <= 0.0) {
        panic("Step size must be positive.")
    }
    var n: Int = (((x_final - x_initial) / step_size).toInt()).toInt()
    var y: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i <= n) {
        y = run { val _tmp = y.toMutableList(); _tmp.add(0.0); _tmp }
        i = i + 1
    }
    _listSet(y, 0, y_initial)
    var xi: Double = x_initial
    var idx: Int = (0).toInt()
    var root2: Double = sqrt(2.0)
    while (idx < n) {
        var k1: Double = step_size * func(xi, y[idx]!!)
        var k2: Double = step_size * func(xi + (step_size / 2.0), y[idx]!! + (k1 / 2.0))
        var k3: Double = step_size * func(xi + (step_size / 2.0), (y[idx]!! + (((0.0 - 0.5) + (1.0 / root2)) * k1)) + ((1.0 - (1.0 / root2)) * k2))
        var k4: Double = step_size * func(xi + step_size, (y[idx]!! - ((1.0 / root2) * k2)) + ((1.0 + (1.0 / root2)) * k3))
        _listSet(y, idx + 1, y[idx]!! + ((((k1 + ((2.0 - root2) * k2)) + ((2.0 + root2) * k3)) + k4) / 6.0))
        xi = xi + step_size
        idx = idx + 1
    }
    return y
}

fun f1(x: Double, y: Double): Double {
    return (x - y) / 2.0
}

fun f2(x: Double, y: Double): Double {
    return x
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(y1[y1.size - 1]!!))
        var y2: MutableList<Double> = runge_kutta_gills(::f2, 0.0 - 1.0, 0.0, 0.2, 0.0)
        println(y2.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
