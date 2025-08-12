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
var forces1: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(1.0, 1.0), mutableListOf(0.0 - 1.0, 2.0))
var location1: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(1.0, 0.0), mutableListOf(10.0, 0.0))
fun _mod(x: Double, m: Double): Double {
    return x - ((((((x / m).toInt())).toDouble())) * m)
}

fun sin_approx(x: Double): Double {
    var y: Double = _mod(x + PI, TWO_PI) - PI
    var y2: Double = y * y
    var y3: Double = y2 * y
    var y5: Double = y3 * y2
    var y7: Double = y5 * y2
    return ((y - (y3 / 6.0)) + (y5 / 120.0)) - (y7 / 5040.0)
}

fun cos_approx(x: Double): Double {
    var y: Double = _mod(x + PI, TWO_PI) - PI
    var y2: Double = y * y
    var y4: Double = y2 * y2
    var y6: Double = y4 * y2
    return ((1.0 - (y2 / 2.0)) + (y4 / 24.0)) - (y6 / 720.0)
}

fun polar_force(magnitude: Double, angle: Double, radian_mode: Boolean): MutableList<Double> {
    var theta: Double = (if (radian_mode != null) angle else (angle * PI) / 180.0.toDouble())
    return mutableListOf(magnitude * cos_approx(theta), magnitude * sin_approx(theta))
}

fun abs_float(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    } else {
        return x
    }
}

fun in_static_equilibrium(forces: MutableList<MutableList<Double>>, location: MutableList<MutableList<Double>>, eps: Double): Boolean {
    var sum_moments: Double = 0.0
    var i: Int = (0).toInt()
    var n: Int = (forces.size).toInt()
    while (i < n) {
        var r: MutableList<Double> = location[i]!!
        var f: MutableList<Double> = forces[i]!!
        var moment: Double = (r[0]!! * f[1]!!) - (r[1]!! * f[0]!!)
        sum_moments = sum_moments + moment
        i = i + 1
    }
    return abs_float(sum_moments) < eps
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(in_static_equilibrium(forces1, location1, 0.1).toString())
        var forces2: MutableList<MutableList<Double>> = mutableListOf(polar_force(718.4, 150.0, false), polar_force(879.54, 45.0, false), polar_force(100.0, 0.0 - 90.0, false))
        var location2: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 0.0), mutableListOf(0.0, 0.0), mutableListOf(0.0, 0.0))
        println(in_static_equilibrium(forces2, location2, 0.1).toString())
        var forces3: MutableList<MutableList<Double>> = mutableListOf(polar_force(30.0 * 9.81, 15.0, false), polar_force(215.0, 135.0, false), polar_force(264.0, 60.0, false))
        var location3: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 0.0), mutableListOf(0.0, 0.0), mutableListOf(0.0, 0.0))
        println(in_static_equilibrium(forces3, location3, 0.1).toString())
        var forces4: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 0.0 - 2000.0), mutableListOf(0.0, 0.0 - 1200.0), mutableListOf(0.0, 15600.0), mutableListOf(0.0, 0.0 - 12400.0))
        var location4: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 0.0), mutableListOf(6.0, 0.0), mutableListOf(10.0, 0.0), mutableListOf(12.0, 0.0))
        println(in_static_equilibrium(forces4, location4, 0.1).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
