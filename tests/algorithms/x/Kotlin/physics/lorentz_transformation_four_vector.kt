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

var c: Double = 299792458.0
fun sqrtApprox(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x / 2.0
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun beta(velocity: Double): Double {
    if (velocity > c) {
        panic("Speed must not exceed light speed 299,792,458 [m/s]!")
    }
    if (velocity < 1.0) {
        panic("Speed must be greater than or equal to 1!")
    }
    return velocity / c
}

fun gamma(velocity: Double): Double {
    var b: Double = beta(velocity)
    return 1.0 / sqrtApprox(1.0 - (b * b))
}

fun transformation_matrix(velocity: Double): MutableList<MutableList<Double>> {
    var g: Double = gamma(velocity)
    var b: Double = beta(velocity)
    return mutableListOf(mutableListOf(g, (0.0 - g) * b, 0.0, 0.0), mutableListOf((0.0 - g) * b, g, 0.0, 0.0), mutableListOf(0.0, 0.0, 1.0, 0.0), mutableListOf(0.0, 0.0, 0.0, 1.0))
}

fun mat_vec_mul(mat: MutableList<MutableList<Double>>, vec: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < 4) {
        var row: MutableList<Double> = mat[i]!!
        var value: Double = (((row[0]!! * vec[0]!!) + (row[1]!! * vec[1]!!)) + (row[2]!! * vec[2]!!)) + (row[3]!! * vec[3]!!)
        res = (res + mutableListOf(value)).toMutableList()
        i = i + 1
    }
    return res
}

fun transform(velocity: Double, event: MutableList<Double>): MutableList<Double> {
    var g: Double = gamma(velocity)
    var b: Double = beta(velocity)
    var ct: Double = event[0]!! * c
    var x: Double = event[1]!!
    return mutableListOf((g * ct) - ((g * b) * x), (((0.0 - g) * b) * ct) + (g * x), event[2]!!, event[3]!!)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(beta(c).toString())
        println(beta(199792458.0).toString())
        println(beta(100000.0).toString())
        println(gamma(4.0).toString())
        println(gamma(100000.0).toString())
        println(gamma(30000000.0).toString())
        println(transformation_matrix(29979245.0).toString())
        var v: MutableList<Double> = transform(29979245.0, mutableListOf(1.0, 2.0, 3.0, 4.0))
        println(v.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
