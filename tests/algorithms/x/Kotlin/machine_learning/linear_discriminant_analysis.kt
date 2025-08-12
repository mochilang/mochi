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
var seed: Int = (1).toInt()
fun rand(): Int {
    seed = (((Math.floorMod((((seed * 1103515245) + 12345).toLong()), 2147483648L)).toInt())).toInt()
    return seed
}

fun random(): Double {
    return (((rand()).toDouble())) / 2147483648.0
}

fun _mod(x: Double, m: Double): Double {
    return x - ((((((x / m).toInt())).toDouble())) * m)
}

fun cos(x: Double): Double {
    var y: Double = _mod(x + PI, TWO_PI) - PI
    var y2: Double = y * y
    var y4: Double = y2 * y2
    var y6: Double = y4 * y2
    return ((1.0 - (y2 / 2.0)) + (y4 / 24.0)) - (y6 / 720.0)
}

fun sqrtApprox(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x
    var i: Int = (0).toInt()
    while (i < 10) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun ln(x: Double): Double {
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

fun gaussian_distribution(mean: Double, std_dev: Double, instance_count: Int): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < instance_count) {
        var u1: Double = random()
        var u2: Double = random()
        var r: Double = sqrtApprox((0.0 - 2.0) * ln(u1))
        var theta: Double = TWO_PI * u2
        var z: Double = r * cos(theta)
        res = run { val _tmp = res.toMutableList(); _tmp.add(mean + (z * std_dev)); _tmp }
        i = i + 1
    }
    return res
}

fun y_generator(class_count: Int, instance_count: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var k: Int = (0).toInt()
    while (k < class_count) {
        var i: Int = (0).toInt()
        while (i < instance_count[k]!!) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(k); _tmp }
            i = i + 1
        }
        k = k + 1
    }
    return res
}

fun calculate_mean(instance_count: Int, items: MutableList<Double>): Double {
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < instance_count) {
        total = total + items[i]!!
        i = i + 1
    }
    return total / ((instance_count.toDouble()))
}

fun calculate_probabilities(instance_count: Int, total_count: Int): Double {
    return ((instance_count.toDouble())) / ((total_count.toDouble()))
}

fun calculate_variance(items: MutableList<MutableList<Double>>, means: MutableList<Double>, total_count: Int): Double {
    var squared_diff: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < items.size) {
        var j: Int = (0).toInt()
        while (j < (items[i]!!).size) {
            var diff: Double = (((items[i]!!) as MutableList<Double>))[j]!! - means[i]!!
            squared_diff = run { val _tmp = squared_diff.toMutableList(); _tmp.add(diff * diff); _tmp }
            j = j + 1
        }
        i = i + 1
    }
    var sum_sq: Double = 0.0
    var k: Int = (0).toInt()
    while (k < squared_diff.size) {
        sum_sq = sum_sq + squared_diff[k]!!
        k = k + 1
    }
    var n_classes: Int = (means.size).toInt()
    return (1.0 / (((total_count - n_classes).toDouble()))) * sum_sq
}

fun predict_y_values(x_items: MutableList<MutableList<Double>>, means: MutableList<Double>, variance: Double, probabilities: MutableList<Double>): MutableList<Int> {
    var results: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < x_items.size) {
        var j: Int = (0).toInt()
        while (j < (x_items[i]!!).size) {
            var temp: MutableList<Double> = mutableListOf<Double>()
            var k: Int = (0).toInt()
            while (k < x_items.size) {
                var discr: Double = (((((x_items[i]!!) as MutableList<Double>))[j]!! * (means[k]!! / variance)) - ((means[k]!! * means[k]!!) / (2.0 * variance))) + ln(probabilities[k]!!)
                temp = run { val _tmp = temp.toMutableList(); _tmp.add(discr); _tmp }
                k = k + 1
            }
            var max_idx: Int = (0).toInt()
            var max_val: Double = temp[0]!!
            var t: Int = (1).toInt()
            while (t < temp.size) {
                if (temp[t]!! > max_val) {
                    max_val = temp[t]!!
                    max_idx = t
                }
                t = t + 1
            }
            results = run { val _tmp = results.toMutableList(); _tmp.add(max_idx); _tmp }
            j = j + 1
        }
        i = i + 1
    }
    return results
}

fun accuracy(actual_y: MutableList<Int>, predicted_y: MutableList<Int>): Double {
    var correct: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < actual_y.size) {
        if (actual_y[i]!! == predicted_y[i]!!) {
            correct = correct + 1
        }
        i = i + 1
    }
    return (((correct.toDouble())) / ((actual_y.size.toDouble()))) * 100.0
}

fun user_main(): Unit {
    seed = (1).toInt()
    var counts: MutableList<Int> = mutableListOf(20, 20, 20)
    var means: MutableList<Double> = mutableListOf(5.0, 10.0, 15.0)
    var std_dev: Double = 1.0
    var x: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < counts.size) {
        x = run { val _tmp = x.toMutableList(); _tmp.add(gaussian_distribution(means[i]!!, std_dev, counts[i]!!)); _tmp }
        i = i + 1
    }
    var y: MutableList<Int> = y_generator(counts.size, counts)
    var actual_means: MutableList<Double> = mutableListOf<Double>()
    i = 0
    while (i < counts.size) {
        actual_means = run { val _tmp = actual_means.toMutableList(); _tmp.add(calculate_mean(counts[i]!!, x[i]!!)); _tmp }
        i = i + 1
    }
    var total_count: Int = (0).toInt()
    i = 0
    while (i < counts.size) {
        total_count = total_count + counts[i]!!
        i = i + 1
    }
    var probabilities: MutableList<Double> = mutableListOf<Double>()
    i = 0
    while (i < counts.size) {
        probabilities = run { val _tmp = probabilities.toMutableList(); _tmp.add(calculate_probabilities(counts[i]!!, total_count)); _tmp }
        i = i + 1
    }
    var variance: Double = calculate_variance(x, actual_means, total_count)
    var predicted: MutableList<Int> = predict_y_values(x, actual_means, variance, probabilities)
    println(predicted)
    println(accuracy(y, predicted))
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
