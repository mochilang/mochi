fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

var data_x: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(1.0, 1.0), mutableListOf(1.0, 2.0), mutableListOf(1.0, 3.0))
var data_y: MutableList<Double> = mutableListOf(1.0, 2.0, 3.0)
var theta: MutableList<Double> = run_linear_regression(data_x, data_y)
fun dot(x: MutableList<Double>, y: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < x.size) {
        sum = sum + (x[i]!! * y[i]!!)
        i = (i + 1).toInt()
    }
    return sum
}

fun run_steep_gradient_descent(data_x: MutableList<MutableList<Double>>, data_y: MutableList<Double>, len_data: Int, alpha: Double, theta: MutableList<Double>): MutableList<Double> {
    var gradients: MutableList<Double> = mutableListOf<Double>()
    var j: Int = (0).toInt()
    while (j < theta.size) {
        gradients = run { val _tmp = gradients.toMutableList(); _tmp.add(0.0); _tmp }
        j = j + 1
    }
    var i: Int = (0).toInt()
    while (i < len_data) {
        var prediction: Double = dot(theta, data_x[i]!!)
        var error: Double = prediction - data_y[i]!!
        var k: Int = (0).toInt()
        while (k < theta.size) {
            _listSet(gradients, k, gradients[k]!! + (error * (((data_x[i]!!) as MutableList<Double>))[k]!!))
            k = k + 1
        }
        i = (i + 1).toInt()
    }
    var t: MutableList<Double> = mutableListOf<Double>()
    var g: Int = (0).toInt()
    while (g < theta.size) {
        t = run { val _tmp = t.toMutableList(); _tmp.add(theta[g]!! - ((alpha / len_data) * gradients[g]!!)); _tmp }
        g = g + 1
    }
    return t
}

fun sum_of_square_error(data_x: MutableList<MutableList<Double>>, data_y: MutableList<Double>, len_data: Int, theta: MutableList<Double>): Double {
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < len_data) {
        var prediction: Double = dot(theta, data_x[i]!!)
        var diff: Double = prediction - data_y[i]!!
        total = total + (diff * diff)
        i = (i + 1).toInt()
    }
    return total / (2.0 * len_data)
}

fun run_linear_regression(data_x: MutableList<MutableList<Double>>, data_y: MutableList<Double>): MutableList<Double> {
    var iterations: Int = (10).toInt()
    var alpha: Double = 0.01
    var no_features: Int = ((data_x[0]!!).size).toInt()
    var len_data: Int = (data_x.size).toInt()
    var theta: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < no_features) {
        theta = run { val _tmp = theta.toMutableList(); _tmp.add(0.0); _tmp }
        i = (i + 1).toInt()
    }
    var iter: Int = (0).toInt()
    while (iter < iterations) {
        theta = run_steep_gradient_descent(data_x, data_y, len_data, alpha, theta)
        var error: Double = sum_of_square_error(data_x, data_y, len_data, theta)
        println((("At Iteration " + (iter + 1).toString()) + " - Error is ") + error.toString())
        iter = iter + 1
    }
    return theta
}

fun absf(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    } else {
        return x
    }
}

fun mean_absolute_error(predicted_y: MutableList<Double>, original_y: MutableList<Double>): Double {
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < predicted_y.size) {
        var diff: Double = absf(predicted_y[i]!! - original_y[i]!!)
        total = total + diff
        i = (i + 1).toInt()
    }
    return total / predicted_y.size
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Resultant Feature vector :")
        var i: Int = (0).toInt()
        while (i < theta.size) {
            println((theta[i]!!).toString())
            i = (i + 1).toInt()
        }
        var predicted_y: MutableList<Double> = mutableListOf(3.0, 0.0 - 0.5, 2.0, 7.0)
        var original_y: MutableList<Double> = mutableListOf(2.5, 0.0, 2.0, 8.0)
        var mae: Double = mean_absolute_error(predicted_y, original_y)
        println("Mean Absolute Error : " + mae.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
