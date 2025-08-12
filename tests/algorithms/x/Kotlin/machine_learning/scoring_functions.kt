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

fun absf(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun sqrtApprox(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun ln_series(x: Double): Double {
    var t: Double = (x - 1.0) / (x + 1.0)
    var term: Double = t
    var sum: Double = 0.0
    var n: Int = (1).toInt()
    while (n <= 19) {
        sum = sum + (term / ((n.toDouble())))
        term = (term * t) * t
        n = n + 2
    }
    return 2.0 * sum
}

fun ln(x: Double): Double {
    var y: Double = x
    var k: Int = (0).toInt()
    while (y >= 10.0) {
        y = y / 10.0
        k = k + 1
    }
    while (y < 1.0) {
        y = y * 10.0
        k = k - 1
    }
    return ln_series(y) + (((k.toDouble())) * ln_series(10.0))
}

fun mae(predict: MutableList<Double>, actual: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < predict.size) {
        var diff: Double = predict[i]!! - actual[i]!!
        sum = sum + absf(diff)
        i = i + 1
    }
    return sum / ((predict.size.toDouble()))
}

fun mse(predict: MutableList<Double>, actual: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < predict.size) {
        var diff: Double = predict[i]!! - actual[i]!!
        sum = sum + (diff * diff)
        i = i + 1
    }
    return sum / ((predict.size.toDouble()))
}

fun rmse(predict: MutableList<Double>, actual: MutableList<Double>): Double {
    return sqrtApprox(mse(predict, actual))
}

fun rmsle(predict: MutableList<Double>, actual: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < predict.size) {
        var lp: Double = ln(predict[i]!! + 1.0)
        var la: Double = ln(actual[i]!! + 1.0)
        var diff: Double = lp - la
        sum = sum + (diff * diff)
        i = i + 1
    }
    return sqrtApprox(sum / ((predict.size.toDouble())))
}

fun mbd(predict: MutableList<Double>, actual: MutableList<Double>): Double {
    var diff_sum: Double = 0.0
    var actual_sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < predict.size) {
        diff_sum = diff_sum + (predict[i]!! - actual[i]!!)
        actual_sum = actual_sum + actual[i]!!
        i = i + 1
    }
    var n: Double = (predict.size.toDouble())
    var numerator: Double = diff_sum / n
    var denominator: Double = actual_sum / n
    return (numerator / denominator) * 100.0
}

fun manual_accuracy(predict: MutableList<Double>, actual: MutableList<Double>): Double {
    var correct: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < predict.size) {
        if (predict[i]!! == actual[i]!!) {
            correct = correct + 1
        }
        i = i + 1
    }
    return ((correct.toDouble())) / ((predict.size.toDouble()))
}

fun user_main(): Unit {
    var actual: MutableList<Double> = mutableListOf(1.0, 2.0, 3.0)
    var predict: MutableList<Double> = mutableListOf(1.0, 4.0, 3.0)
    println(mae(predict, actual).toString())
    println(mse(predict, actual).toString())
    println(rmse(predict, actual).toString())
    println(rmsle(mutableListOf(10.0, 2.0, 30.0), mutableListOf(10.0, 10.0, 30.0)).toString())
    println(mbd(mutableListOf(2.0, 3.0, 4.0), mutableListOf(1.0, 2.0, 3.0)).toString())
    println(mbd(mutableListOf(0.0, 1.0, 1.0), mutableListOf(1.0, 2.0, 3.0)).toString())
    println(manual_accuracy(predict, actual).toString())
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
