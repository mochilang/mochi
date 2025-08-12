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

data class DataPoint(var x: MutableList<Double> = mutableListOf<Double>(), var y: Double = 0.0)
var train_data: MutableList<DataPoint> = mutableListOf(DataPoint(x = mutableListOf(5.0, 2.0, 3.0), y = 15.0), DataPoint(x = mutableListOf(6.0, 5.0, 9.0), y = 25.0), DataPoint(x = mutableListOf(11.0, 12.0, 13.0), y = 41.0), DataPoint(x = mutableListOf(1.0, 1.0, 1.0), y = 8.0), DataPoint(x = mutableListOf(11.0, 12.0, 13.0), y = 41.0))
var test_data: MutableList<DataPoint> = mutableListOf(DataPoint(x = mutableListOf(515.0, 22.0, 13.0), y = 555.0), DataPoint(x = mutableListOf(61.0, 35.0, 49.0), y = 150.0))
var parameter_vector: MutableList<Double> = mutableListOf(2.0, 4.0, 1.0, 5.0)
fun absf(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun hypothesis_value(input: MutableList<Double>, params: MutableList<Double>): Double {
    var value: Double = params[0]!!
    var i: Int = (0).toInt()
    while (i < input.size) {
        value = value + (input[i]!! * params[i + 1]!!)
        i = i + 1
    }
    return value
}

fun calc_error(dp: DataPoint, params: MutableList<Double>): Double {
    return hypothesis_value(dp.x, params) - dp.y
}

fun summation_of_cost_derivative(index: Int, params: MutableList<Double>, data: MutableList<DataPoint>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < data.size) {
        var dp: DataPoint = data[i]!!
        var e: Double = calc_error(dp, params)
        if (index == (0 - 1)) {
            sum = sum + e
        } else {
            sum = sum + (e * (dp.x)[index]!!)
        }
        i = i + 1
    }
    return sum
}

fun get_cost_derivative(index: Int, params: MutableList<Double>, data: MutableList<DataPoint>): Double {
    return summation_of_cost_derivative(index, params, data) / ((data.size.toDouble()))
}

fun allclose(a: MutableList<Double>, b: MutableList<Double>, atol: Double, rtol: Double): Boolean {
    var i: Int = (0).toInt()
    while (i < a.size) {
        var diff: Double = absf(a[i]!! - b[i]!!)
        var limit: Double = atol + (rtol * absf(b[i]!!))
        if (diff > limit) {
            return false
        }
        i = i + 1
    }
    return true
}

fun run_gradient_descent(train_data: MutableList<DataPoint>, initial_params: MutableList<Double>): MutableList<Double> {
    var learning_rate: Double = 0.009
    var absolute_error_limit: Double = 0.000002
    var relative_error_limit: Double = 0.0
    var j: Int = (0).toInt()
    var params: MutableList<Double> = initial_params
    while (true) {
        j = j + 1
        var temp: MutableList<Double> = mutableListOf<Double>()
        var i: Int = (0).toInt()
        while (i < params.size) {
            var deriv: Double = get_cost_derivative(i - 1, params, train_data)
            temp = run { val _tmp = temp.toMutableList(); _tmp.add(params[i]!! - (learning_rate * deriv)); _tmp }
            i = i + 1
        }
        if (((allclose(params, temp, absolute_error_limit, relative_error_limit)) as Boolean)) {
            println("Number of iterations:" + j.toString())
            break
        }
        params = temp
    }
    return params
}

fun test_gradient_descent(test_data: MutableList<DataPoint>, params: MutableList<Double>): Unit {
    var i: Int = (0).toInt()
    while (i < test_data.size) {
        var dp: DataPoint = test_data[i]!!
        println("Actual output value:" + dp.y.toString())
        println("Hypothesis output:" + hypothesis_value(dp.x, params).toString())
        i = i + 1
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        parameter_vector = run_gradient_descent(train_data, parameter_vector)
        println("\nTesting gradient descent for a linear hypothesis function.\n")
        test_gradient_descent(test_data, parameter_vector)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
