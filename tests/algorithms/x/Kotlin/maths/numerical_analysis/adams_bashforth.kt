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

var y2: MutableList<Double> = adams_bashforth_step2(::f_x, mutableListOf(0.0, 0.2), mutableListOf(0.0, 0.0), 0.2, 1.0)
fun abs_float(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    } else {
        return x
    }
}

fun validate_inputs(x_initials: MutableList<Double>, step_size: Double, x_final: Double): Unit {
    if (x_initials[x_initials.size - 1]!! >= x_final) {
        panic("The final value of x must be greater than the initial values of x.")
    }
    if (step_size <= 0.0) {
        panic("Step size must be positive.")
    }
    var i: Int = (0).toInt()
    while (i < (x_initials.size - 1)) {
        var diff: Double = x_initials[i + 1]!! - x_initials[i]!!
        if (abs_float(diff - step_size) > 0.0000000001) {
            panic("x-values must be equally spaced according to step size.")
        }
        i = i + 1
    }
}

fun list_to_string(xs: MutableList<Double>): String {
    var s: String = "["
    var i: Int = (0).toInt()
    while (i < xs.size) {
        s = s + _numToStr(xs[i]!!)
        if ((i + 1) < xs.size) {
            s = s + ", "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun adams_bashforth_step2(f: (Double, Double) -> Double, x_initials: MutableList<Double>, y_initials: MutableList<Double>, step_size: Double, x_final: Double): MutableList<Double> {
    validate_inputs(x_initials, step_size, x_final)
    if ((x_initials.size != 2) || (y_initials.size != 2)) {
        panic("Insufficient initial points information.")
    }
    var x0: Double = x_initials[0]!!
    var x1: Double = x_initials[1]!!
    var y: MutableList<Double> = mutableListOf<Double>()
    y = run { val _tmp = y.toMutableList(); _tmp.add(y_initials[0]!!); _tmp }
    y = run { val _tmp = y.toMutableList(); _tmp.add(y_initials[1]!!); _tmp }
    var n: Int = (((x_final - x1) / step_size).toInt()).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        var term: Double = (3.0 * f(x1, y[i + 1]!!)) - f(x0, y[i]!!)
        var y_next: Double = y[i + 1]!! + ((step_size / 2.0) * term)
        y = run { val _tmp = y.toMutableList(); _tmp.add(y_next); _tmp }
        x0 = x1
        x1 = x1 + step_size
        i = i + 1
    }
    return y
}

fun adams_bashforth_step3(f: (Double, Double) -> Double, x_initials: MutableList<Double>, y_initials: MutableList<Double>, step_size: Double, x_final: Double): MutableList<Double> {
    validate_inputs(x_initials, step_size, x_final)
    if ((x_initials.size != 3) || (y_initials.size != 3)) {
        panic("Insufficient initial points information.")
    }
    var x0: Double = x_initials[0]!!
    var x1: Double = x_initials[1]!!
    var x2: Double = x_initials[2]!!
    var y: MutableList<Double> = mutableListOf<Double>()
    y = run { val _tmp = y.toMutableList(); _tmp.add(y_initials[0]!!); _tmp }
    y = run { val _tmp = y.toMutableList(); _tmp.add(y_initials[1]!!); _tmp }
    y = run { val _tmp = y.toMutableList(); _tmp.add(y_initials[2]!!); _tmp }
    var n: Int = (((x_final - x2) / step_size).toInt()).toInt()
    var i: Int = (0).toInt()
    while (i <= n) {
        var term: Double = ((23.0 * f(x2, y[i + 2]!!)) - (16.0 * f(x1, y[i + 1]!!))) + (5.0 * f(x0, y[i]!!))
        var y_next: Double = y[i + 2]!! + ((step_size / 12.0) * term)
        y = run { val _tmp = y.toMutableList(); _tmp.add(y_next); _tmp }
        x0 = x1
        x1 = x2
        x2 = x2 + step_size
        i = i + 1
    }
    return y
}

fun adams_bashforth_step4(f: (Double, Double) -> Double, x_initials: MutableList<Double>, y_initials: MutableList<Double>, step_size: Double, x_final: Double): MutableList<Double> {
    validate_inputs(x_initials, step_size, x_final)
    if ((x_initials.size != 4) || (y_initials.size != 4)) {
        panic("Insufficient initial points information.")
    }
    var x0: Double = x_initials[0]!!
    var x1: Double = x_initials[1]!!
    var x2: Double = x_initials[2]!!
    var x3: Double = x_initials[3]!!
    var y: MutableList<Double> = mutableListOf<Double>()
    y = run { val _tmp = y.toMutableList(); _tmp.add(y_initials[0]!!); _tmp }
    y = run { val _tmp = y.toMutableList(); _tmp.add(y_initials[1]!!); _tmp }
    y = run { val _tmp = y.toMutableList(); _tmp.add(y_initials[2]!!); _tmp }
    y = run { val _tmp = y.toMutableList(); _tmp.add(y_initials[3]!!); _tmp }
    var n: Int = (((x_final - x3) / step_size).toInt()).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        var term: Double = (((55.0 * f(x3, y[i + 3]!!)) - (59.0 * f(x2, y[i + 2]!!))) + (37.0 * f(x1, y[i + 1]!!))) - (9.0 * f(x0, y[i]!!))
        var y_next: Double = y[i + 3]!! + ((step_size / 24.0) * term)
        y = run { val _tmp = y.toMutableList(); _tmp.add(y_next); _tmp }
        x0 = x1
        x1 = x2
        x2 = x3
        x3 = x3 + step_size
        i = i + 1
    }
    return y
}

fun adams_bashforth_step5(f: (Double, Double) -> Double, x_initials: MutableList<Double>, y_initials: MutableList<Double>, step_size: Double, x_final: Double): MutableList<Double> {
    validate_inputs(x_initials, step_size, x_final)
    if ((x_initials.size != 5) || (y_initials.size != 5)) {
        panic("Insufficient initial points information.")
    }
    var x0: Double = x_initials[0]!!
    var x1: Double = x_initials[1]!!
    var x2: Double = x_initials[2]!!
    var x3: Double = x_initials[3]!!
    var x4: Double = x_initials[4]!!
    var y: MutableList<Double> = mutableListOf<Double>()
    y = run { val _tmp = y.toMutableList(); _tmp.add(y_initials[0]!!); _tmp }
    y = run { val _tmp = y.toMutableList(); _tmp.add(y_initials[1]!!); _tmp }
    y = run { val _tmp = y.toMutableList(); _tmp.add(y_initials[2]!!); _tmp }
    y = run { val _tmp = y.toMutableList(); _tmp.add(y_initials[3]!!); _tmp }
    y = run { val _tmp = y.toMutableList(); _tmp.add(y_initials[4]!!); _tmp }
    var n: Int = (((x_final - x4) / step_size).toInt()).toInt()
    var i: Int = (0).toInt()
    while (i <= n) {
        var term: Double = ((((1901.0 * f(x4, y[i + 4]!!)) - (2774.0 * f(x3, y[i + 3]!!))) - (2616.0 * f(x2, y[i + 2]!!))) - (1274.0 * f(x1, y[i + 1]!!))) + (251.0 * f(x0, y[i]!!))
        var y_next: Double = y[i + 4]!! + ((step_size / 720.0) * term)
        y = run { val _tmp = y.toMutableList(); _tmp.add(y_next); _tmp }
        x0 = x1
        x1 = x2
        x2 = x3
        x3 = x4
        x4 = x4 + step_size
        i = i + 1
    }
    return y
}

fun f_x(x: Double, y: Double): Double {
    return x
}

fun f_xy(x: Double, y: Double): Double {
    return x + y
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(list_to_string(y2))
        var y3: MutableList<Double> = adams_bashforth_step3(::f_xy, mutableListOf(0.0, 0.2, 0.4), mutableListOf(0.0, 0.0, 0.04), 0.2, 1.0)
        println(_numToStr(y3[3]!!))
        var y4: MutableList<Double> = adams_bashforth_step4(::f_xy, mutableListOf(0.0, 0.2, 0.4, 0.6), mutableListOf(0.0, 0.0, 0.04, 0.128), 0.2, 1.0)
        println(_numToStr(y4[4]!!))
        println(_numToStr(y4[5]!!))
        var y5: MutableList<Double> = adams_bashforth_step5(::f_xy, mutableListOf(0.0, 0.2, 0.4, 0.6, 0.8), mutableListOf(0.0, 0.0214, 0.0214, 0.22211, 0.42536), 0.2, 1.0)
        println(_numToStr(y5[y5.size - 1]!!))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
