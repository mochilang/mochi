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

fun key(x: Int, y: Int): String {
    return (_numToStr(x) + ",") + _numToStr(y)
}

fun joint_probability_distribution(x_values: MutableList<Int>, y_values: MutableList<Int>, x_probabilities: MutableList<Double>, y_probabilities: MutableList<Double>): MutableMap<String, Double> {
    var result: MutableMap<String, Double> = mutableMapOf<String, Double>()
    var i: Int = (0).toInt()
    while (i < x_values.size) {
        var j: Int = (0).toInt()
        while (j < y_values.size) {
            var k: String = key(x_values[i]!!, y_values[j]!!)
            (result)[k] = x_probabilities[i]!! * y_probabilities[j]!!
            j = j + 1
        }
        i = i + 1
    }
    return result
}

fun expectation(values: MutableList<Int>, probabilities: MutableList<Double>): Double {
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < values.size) {
        total = total + (((values[i]!!).toDouble()) * probabilities[i]!!)
        i = i + 1
    }
    return total
}

fun variance(values: MutableList<Int>, probabilities: MutableList<Double>): Double {
    var mean: Double = expectation(values, probabilities)
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < values.size) {
        var diff: Double = ((values[i]!!).toDouble()) - mean
        total = total + ((diff * diff) * probabilities[i]!!)
        i = i + 1
    }
    return total
}

fun covariance(x_values: MutableList<Int>, y_values: MutableList<Int>, x_probabilities: MutableList<Double>, y_probabilities: MutableList<Double>): Double {
    var mean_x: Double = expectation(x_values, x_probabilities)
    var mean_y: Double = expectation(y_values, y_probabilities)
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < x_values.size) {
        var j: Int = (0).toInt()
        while (j < y_values.size) {
            var diff_x: Double = ((x_values[i]!!).toDouble()) - mean_x
            var diff_y: Double = ((y_values[j]!!).toDouble()) - mean_y
            total = total + (((diff_x * diff_y) * x_probabilities[i]!!) * y_probabilities[j]!!)
            j = j + 1
        }
        i = i + 1
    }
    return total
}

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

fun standard_deviation(v: Double): Double {
    return sqrtApprox(v)
}

fun user_main(): Unit {
    var x_values: MutableList<Int> = mutableListOf(1, 2)
    var y_values: MutableList<Int> = mutableListOf(0 - 2, 5, 8)
    var x_probabilities: MutableList<Double> = mutableListOf(0.7, 0.3)
    var y_probabilities: MutableList<Double> = mutableListOf(0.3, 0.5, 0.2)
    var jpd: MutableMap<String, Double> = joint_probability_distribution(x_values, y_values, x_probabilities, y_probabilities)
    var i: Int = (0).toInt()
    while (i < x_values.size) {
        var j: Int = (0).toInt()
        while (j < y_values.size) {
            var k: String = key(x_values[i]!!, y_values[j]!!)
            var prob: Double = (jpd)[k] as Double
            println((k + "=") + _numToStr(prob))
            j = j + 1
        }
        i = i + 1
    }
    var ex: Double = expectation(x_values, x_probabilities)
    var ey: Double = expectation(y_values, y_probabilities)
    var vx: Double = variance(x_values, x_probabilities)
    var vy: Double = variance(y_values, y_probabilities)
    var cov: Double = covariance(x_values, y_values, x_probabilities, y_probabilities)
    println("Ex=" + _numToStr(ex))
    println("Ey=" + _numToStr(ey))
    println("Vx=" + _numToStr(vx))
    println("Vy=" + _numToStr(vy))
    println("Cov=" + _numToStr(cov))
    println("Sx=" + _numToStr(standard_deviation(vx)))
    println("Sy=" + _numToStr(standard_deviation(vy)))
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
