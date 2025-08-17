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

fun runge_kutta_fehlberg_45(func: (Double, Double) -> Double, x_initial: Double, y_initial: Double, step_size: Double, x_final: Double): MutableList<Double> {
    if (x_initial >= x_final) {
        panic("The final value of x must be greater than initial value of x.")
    }
    if (step_size <= 0.0) {
        panic("Step size must be positive.")
    }
    var n: Int = (((x_final - x_initial) / step_size).toInt()).toInt()
    var ys: MutableList<Double> = mutableListOf<Double>()
    var x: Double = x_initial
    var y: Double = y_initial
    ys = run { val _tmp = ys.toMutableList(); _tmp.add(y); _tmp }
    var i: Int = (0).toInt()
    while (i < n) {
        var k1: Double = step_size * func(x, y)
        var k2: Double = step_size * func(x + (step_size / 4.0), y + (k1 / 4.0))
        var k3: Double = step_size * func(x + ((3.0 / 8.0) * step_size), (y + ((3.0 / 32.0) * k1)) + ((9.0 / 32.0) * k2))
        var k4: Double = step_size * func(x + ((12.0 / 13.0) * step_size), ((y + ((1932.0 / 2197.0) * k1)) - ((7200.0 / 2197.0) * k2)) + ((7296.0 / 2197.0) * k3))
        var k5: Double = step_size * func(x + step_size, (((y + ((439.0 / 216.0) * k1)) - (8.0 * k2)) + ((3680.0 / 513.0) * k3)) - ((845.0 / 4104.0) * k4))
        var k6: Double = step_size * func(x + (step_size / 2.0), ((((y - ((8.0 / 27.0) * k1)) + (2.0 * k2)) - ((3544.0 / 2565.0) * k3)) + ((1859.0 / 4104.0) * k4)) - ((11.0 / 40.0) * k5))
        y = ((((y + ((16.0 / 135.0) * k1)) + ((6656.0 / 12825.0) * k3)) + ((28561.0 / 56430.0) * k4)) - ((9.0 / 50.0) * k5)) + ((2.0 / 55.0) * k6)
        x = x + step_size
        ys = run { val _tmp = ys.toMutableList(); _tmp.add(y); _tmp }
        i = i + 1
    }
    return ys
}

fun user_main(): Unit {
    fun f1(x: Double, y: Double): Double {
        return (1.0 + ((y * y).toDouble())).toDouble()
    }

    var y1: MutableList<Double> = runge_kutta_fehlberg_45(::f1, 0.0, 0.0, 0.2, 1.0)
    println(y1[1]!!)
    fun f2(x: Double, y: Double): Double {
        return x
    }

    var y2: MutableList<Double> = runge_kutta_fehlberg_45(::f2, 0.0 - 1.0, 0.0, 0.2, 0.0)
    println(y2[1]!!)
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
