import java.math.BigInteger

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

var control: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(1.0, 1.0), mutableListOf(1.0, 2.0))
fun n_choose_k(n: Int, k: Int): Double {
    if ((k < 0) || (k > n)) {
        return 0.0
    }
    if ((k == 0) || (k == n)) {
        return 1.0
    }
    var result: Double = 1.0
    var i: Int = (1).toInt()
    while (i <= k) {
        result = (result * (1.0 * (((n - k) + i).toDouble()))) / (1.0 * (i).toDouble())
        i = i + 1
    }
    return result
}

fun pow_float(base: Double, exp: Int): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun basis_function(points: MutableList<MutableList<Double>>, t: Double): MutableList<Double> {
    var degree: Int = (points.size - 1).toInt()
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i <= degree) {
        var coef: Double = n_choose_k(degree, i)
        var term: Double = pow_float(1.0 - t, degree - i) * pow_float(t, i)
        res = run { val _tmp = res.toMutableList(); _tmp.add(coef * term); _tmp }
        i = i + 1
    }
    return res
}

fun bezier_point(points: MutableList<MutableList<Double>>, t: Double): MutableList<Double> {
    var basis: MutableList<Double> = basis_function(points, t)
    var x: Double = 0.0
    var y: Double = 0.0
    var i: Int = (0).toInt()
    while (i < points.size) {
        x = x + (basis[i]!! * (((points[i]!!) as MutableList<Double>))[0]!!)
        y = y + (basis[i]!! * (((points[i]!!) as MutableList<Double>))[1]!!)
        i = i + 1
    }
    return mutableListOf(x, y)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(basis_function(control, 0.0).toString())
        println(basis_function(control, 1.0).toString())
        println(bezier_point(control, 0.0).toString())
        println(bezier_point(control, 1.0).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
