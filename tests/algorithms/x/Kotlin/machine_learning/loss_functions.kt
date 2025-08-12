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

fun absf(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun maxf(a: Double, b: Double): Double {
    if (a > b) {
        return a
    }
    return b
}

fun minf(a: Double, b: Double): Double {
    if (a < b) {
        return a
    }
    return b
}

fun clip(x: Double, lo: Double, hi: Double): Double {
    return maxf(lo, minf(x, hi))
}

fun to_float(x: Int): Double {
    return x * 1.0
}

fun powf(base: Double, exp: Double): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    var n: Int = ((exp.toInt())).toInt()
    while (i < n) {
        result = result * base
        i = i + 1
    }
    return result
}

fun ln(x: Double): Double {
    if (x <= 0.0) {
        panic("ln domain error")
    }
    var y: Double = (x - 1.0) / (x + 1.0)
    var y2: Double = y * y
    var term: Double = y
    var sum: Double = 0.0
    var k: Int = (0).toInt()
    while (k < 10) {
        var denom: Double = to_float((2 * k) + 1)
        sum = sum + (term / denom)
        term = term * y2
        k = k + 1
    }
    return 2.0 * sum
}

fun exp(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = (1).toInt()
    while (n < 20) {
        term = (term * x) / to_float(n)
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun mean(v: MutableList<Double>): Double {
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < v.size) {
        total = total + v[i]!!
        i = i + 1
    }
    return total / to_float(v.size)
}

fun binary_cross_entropy(y_true: MutableList<Double>, y_pred: MutableList<Double>, epsilon: Double): Double {
    if (y_true.size != y_pred.size) {
        panic("Input arrays must have the same length.")
    }
    var losses: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < y_true.size) {
        var yt: Double = y_true[i]!!
        var yp: Double = clip(y_pred[i]!!, epsilon, 1.0 - epsilon)
        var loss: Double = 0.0 - ((yt * ln(yp)) + ((1.0 - yt) * ln(1.0 - yp)))
        losses = run { val _tmp = losses.toMutableList(); _tmp.add(loss); _tmp }
        i = i + 1
    }
    return mean(losses)
}

fun binary_focal_cross_entropy(y_true: MutableList<Double>, y_pred: MutableList<Double>, gamma: Double, alpha: Double, epsilon: Double): Double {
    if (y_true.size != y_pred.size) {
        panic("Input arrays must have the same length.")
    }
    var losses: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < y_true.size) {
        var yt: Double = y_true[i]!!
        var yp: Double = clip(y_pred[i]!!, epsilon, 1.0 - epsilon)
        var term1: Double = ((alpha * powf(1.0 - yp, gamma)) * yt) * ln(yp)
        var term2: Double = (((1.0 - alpha) * powf(yp, gamma)) * (1.0 - yt)) * ln(1.0 - yp)
        losses = run { val _tmp = losses.toMutableList(); _tmp.add(0.0 - (term1 + term2)); _tmp }
        i = i + 1
    }
    return mean(losses)
}

fun categorical_cross_entropy(y_true: MutableList<MutableList<Double>>, y_pred: MutableList<MutableList<Double>>, epsilon: Double): Double {
    if (y_true.size != y_pred.size) {
        panic("Input arrays must have the same shape.")
    }
    var rows: Int = (y_true.size).toInt()
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < rows) {
        if ((y_true[i]!!).size != (y_pred[i]!!).size) {
            panic("Input arrays must have the same shape.")
        }
        var sum_true: Double = 0.0
        var sum_pred: Double = 0.0
        var j: Int = (0).toInt()
        while (j < (y_true[i]!!).size) {
            var yt: Double = (((y_true[i]!!) as MutableList<Double>))[j]!!
            var yp: Double = (((y_pred[i]!!) as MutableList<Double>))[j]!!
            if ((yt != 0.0) && (yt != 1.0)) {
                panic("y_true must be one-hot encoded.")
            }
            sum_true = sum_true + yt
            sum_pred = sum_pred + yp
            j = j + 1
        }
        if (sum_true != 1.0) {
            panic("y_true must be one-hot encoded.")
        }
        if (absf(sum_pred - 1.0) > epsilon) {
            panic("Predicted probabilities must sum to approximately 1.")
        }
        j = 0
        while (j < (y_true[i]!!).size) {
            var yp: Double = clip((((y_pred[i]!!) as MutableList<Double>))[j]!!, epsilon, 1.0)
            total = total - ((((y_true[i]!!) as MutableList<Double>))[j]!! * ln(yp))
            j = j + 1
        }
        i = i + 1
    }
    return total
}

fun categorical_focal_cross_entropy(y_true: MutableList<MutableList<Double>>, y_pred: MutableList<MutableList<Double>>, alpha: MutableList<Double>, gamma: Double, epsilon: Double): Double {
    if (y_true.size != y_pred.size) {
        panic("Shape of y_true and y_pred must be the same.")
    }
    var rows: Int = (y_true.size).toInt()
    var cols: Int = ((y_true[0]!!).size).toInt()
    var a: MutableList<Double> = alpha
    if (a.size == 0) {
        var tmp: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < cols) {
            tmp = run { val _tmp = tmp.toMutableList(); _tmp.add(1.0); _tmp }
            j = j + 1
        }
        a = tmp
    }
    if (a.size != cols) {
        panic("Length of alpha must match the number of classes.")
    }
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < rows) {
        if (((y_true[i]!!).size != cols) || ((y_pred[i]!!).size != cols)) {
            panic("Shape of y_true and y_pred must be the same.")
        }
        var sum_true: Double = 0.0
        var sum_pred: Double = 0.0
        var j: Int = (0).toInt()
        while (j < cols) {
            var yt: Double = (((y_true[i]!!) as MutableList<Double>))[j]!!
            var yp: Double = (((y_pred[i]!!) as MutableList<Double>))[j]!!
            if ((yt != 0.0) && (yt != 1.0)) {
                panic("y_true must be one-hot encoded.")
            }
            sum_true = sum_true + yt
            sum_pred = sum_pred + yp
            j = j + 1
        }
        if (sum_true != 1.0) {
            panic("y_true must be one-hot encoded.")
        }
        if (absf(sum_pred - 1.0) > epsilon) {
            panic("Predicted probabilities must sum to approximately 1.")
        }
        var row_loss: Double = 0.0
        j = 0
        while (j < cols) {
            var yp: Double = clip((((y_pred[i]!!) as MutableList<Double>))[j]!!, epsilon, 1.0)
            row_loss = row_loss + (((a[j]!! * powf(1.0 - yp, gamma)) * (((y_true[i]!!) as MutableList<Double>))[j]!!) * ln(yp))
            j = j + 1
        }
        total = total - row_loss
        i = i + 1
    }
    return total / to_float(rows)
}

fun hinge_loss(y_true: MutableList<Double>, y_pred: MutableList<Double>): Double {
    if (y_true.size != y_pred.size) {
        panic("Length of predicted and actual array must be same.")
    }
    var losses: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < y_true.size) {
        var yt: Double = y_true[i]!!
        if ((yt != (0.0 - 1.0)) && (yt != 1.0)) {
            panic("y_true can have values -1 or 1 only.")
        }
        var pred: Double = y_pred[i]!!
        var l: Double = maxf(0.0, 1.0 - (yt * pred))
        losses = run { val _tmp = losses.toMutableList(); _tmp.add(l); _tmp }
        i = i + 1
    }
    return mean(losses)
}

fun huber_loss(y_true: MutableList<Double>, y_pred: MutableList<Double>, delta: Double): Double {
    if (y_true.size != y_pred.size) {
        panic("Input arrays must have the same length.")
    }
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < y_true.size) {
        var diff: Double = y_true[i]!! - y_pred[i]!!
        var adiff: Double = absf(diff)
        if (adiff <= delta) {
            total = total + ((0.5 * diff) * diff)
        } else {
            total = total + (delta * (adiff - (0.5 * delta)))
        }
        i = i + 1
    }
    return total / to_float(y_true.size)
}

fun mean_squared_error(y_true: MutableList<Double>, y_pred: MutableList<Double>): Double {
    if (y_true.size != y_pred.size) {
        panic("Input arrays must have the same length.")
    }
    var losses: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < y_true.size) {
        var diff: Double = y_true[i]!! - y_pred[i]!!
        losses = run { val _tmp = losses.toMutableList(); _tmp.add(diff * diff); _tmp }
        i = i + 1
    }
    return mean(losses)
}

fun mean_absolute_error(y_true: MutableList<Double>, y_pred: MutableList<Double>): Double {
    if (y_true.size != y_pred.size) {
        panic("Input arrays must have the same length.")
    }
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < y_true.size) {
        total = total + absf(y_true[i]!! - y_pred[i]!!)
        i = i + 1
    }
    return total / to_float(y_true.size)
}

fun mean_squared_logarithmic_error(y_true: MutableList<Double>, y_pred: MutableList<Double>): Double {
    if (y_true.size != y_pred.size) {
        panic("Input arrays must have the same length.")
    }
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < y_true.size) {
        var a: Double = ln(1.0 + y_true[i]!!)
        var b: Double = ln(1.0 + y_pred[i]!!)
        var diff: Double = a - b
        total = total + (diff * diff)
        i = i + 1
    }
    return total / to_float(y_true.size)
}

fun mean_absolute_percentage_error(y_true: MutableList<Double>, y_pred: MutableList<Double>, epsilon: Double): Double {
    if (y_true.size != y_pred.size) {
        panic("The length of the two arrays should be the same.")
    }
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < y_true.size) {
        var yt: Double = y_true[i]!!
        if (yt == 0.0) {
            yt = epsilon
        }
        total = total + absf((yt - y_pred[i]!!) / yt)
        i = i + 1
    }
    return total / to_float(y_true.size)
}

fun perplexity_loss(y_true: MutableList<MutableList<Int>>, y_pred: MutableList<MutableList<MutableList<Double>>>, epsilon: Double): Double {
    var batch: Int = (y_true.size).toInt()
    if (batch != y_pred.size) {
        panic("Batch size of y_true and y_pred must be equal.")
    }
    var sentence_len: Int = ((y_true[0]!!).size).toInt()
    if (sentence_len != (y_pred[0]!!).size) {
        panic("Sentence length of y_true and y_pred must be equal.")
    }
    var vocab_size: Int = (((((y_pred[0]!!) as MutableList<MutableList<Double>>))[0]!!).size).toInt()
    var b: Int = (0).toInt()
    var total_perp: Double = 0.0
    while (b < batch) {
        if (((y_true[b]!!).size != sentence_len) || ((y_pred[b]!!).size != sentence_len)) {
            panic("Sentence length of y_true and y_pred must be equal.")
        }
        var sum_log: Double = 0.0
        var j: Int = (0).toInt()
        while (j < sentence_len) {
            var label: Int = ((((y_true[b]!!) as MutableList<Int>))[j]!!).toInt()
            if (label >= vocab_size) {
                panic("Label value must not be greater than vocabulary size.")
            }
            var prob: Double = clip(((((((y_pred[b]!!) as MutableList<MutableList<Double>>))[j]!!) as MutableList<Double>))[label]!!, epsilon, 1.0)
            sum_log = sum_log + ln(prob)
            j = j + 1
        }
        var mean_log: Double = sum_log / to_float(sentence_len)
        var perp: Double = exp(0.0 - mean_log)
        total_perp = total_perp + perp
        b = b + 1
    }
    return total_perp / to_float(batch)
}

fun smooth_l1_loss(y_true: MutableList<Double>, y_pred: MutableList<Double>, beta: Double): Double {
    if (y_true.size != y_pred.size) {
        panic("The length of the two arrays should be the same.")
    }
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < y_true.size) {
        var diff: Double = absf(y_true[i]!! - y_pred[i]!!)
        if (diff < beta) {
            total = total + (((0.5 * diff) * diff) / beta)
        } else {
            total = (total + diff) - (0.5 * beta)
        }
        i = i + 1
    }
    return total / to_float(y_true.size)
}

fun kullback_leibler_divergence(y_true: MutableList<Double>, y_pred: MutableList<Double>): Double {
    if (y_true.size != y_pred.size) {
        panic("Input arrays must have the same length.")
    }
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < y_true.size) {
        total = total + (y_true[i]!! * ln(y_true[i]!! / y_pred[i]!!))
        i = i + 1
    }
    return total
}

fun user_main(): Unit {
    var y_true_bc: MutableList<Double> = mutableListOf(0.0, 1.0, 1.0, 0.0, 1.0)
    var y_pred_bc: MutableList<Double> = mutableListOf(0.2, 0.7, 0.9, 0.3, 0.8)
    println(binary_cross_entropy(y_true_bc, y_pred_bc, 0.000000000000001))
    println(binary_focal_cross_entropy(y_true_bc, y_pred_bc, 2.0, 0.25, 0.000000000000001))
    var y_true_cce: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(1.0, 0.0, 0.0), mutableListOf(0.0, 1.0, 0.0), mutableListOf(0.0, 0.0, 1.0))
    var y_pred_cce: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.9, 0.1, 0.0), mutableListOf(0.2, 0.7, 0.1), mutableListOf(0.0, 0.1, 0.9))
    println(categorical_cross_entropy(y_true_cce, y_pred_cce, 0.000000000000001))
    var alpha: MutableList<Double> = mutableListOf(0.6, 0.2, 0.7)
    println(categorical_focal_cross_entropy(y_true_cce, y_pred_cce, alpha, 2.0, 0.000000000000001))
    var y_true_hinge: MutableList<Double> = mutableListOf(0.0 - 1.0, 1.0, 1.0, 0.0 - 1.0, 1.0)
    var y_pred_hinge: MutableList<Double> = mutableListOf(0.0 - 4.0, 0.0 - 0.3, 0.7, 5.0, 10.0)
    println(hinge_loss(y_true_hinge, y_pred_hinge))
    var y_true_huber: MutableList<Double> = mutableListOf(0.9, 10.0, 2.0, 1.0, 5.2)
    var y_pred_huber: MutableList<Double> = mutableListOf(0.8, 2.1, 2.9, 4.2, 5.2)
    println(huber_loss(y_true_huber, y_pred_huber, 1.0))
    println(mean_squared_error(y_true_huber, y_pred_huber))
    println(mean_absolute_error(y_true_huber, y_pred_huber))
    println(mean_squared_logarithmic_error(y_true_huber, y_pred_huber))
    var y_true_mape: MutableList<Double> = mutableListOf(10.0, 20.0, 30.0, 40.0)
    var y_pred_mape: MutableList<Double> = mutableListOf(12.0, 18.0, 33.0, 45.0)
    println(mean_absolute_percentage_error(y_true_mape, y_pred_mape, 0.000000000000001))
    var y_true_perp: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 4), mutableListOf(2, 3))
    var y_pred_perp: MutableList<MutableList<MutableList<Double>>> = mutableListOf(mutableListOf(mutableListOf(0.28, 0.19, 0.21, 0.15, 0.17), mutableListOf(0.24, 0.19, 0.09, 0.18, 0.3)), mutableListOf(mutableListOf(0.03, 0.26, 0.21, 0.18, 0.32), mutableListOf(0.28, 0.1, 0.33, 0.15, 0.14)))
    println(perplexity_loss(y_true_perp, y_pred_perp, 0.0000001))
    var y_true_smooth: MutableList<Double> = mutableListOf(3.0, 5.0, 2.0, 7.0)
    var y_pred_smooth: MutableList<Double> = mutableListOf(2.9, 4.8, 2.1, 7.2)
    println(smooth_l1_loss(y_true_smooth, y_pred_smooth, 1.0))
    var y_true_kl: MutableList<Double> = mutableListOf(0.2, 0.3, 0.5)
    var y_pred_kl: MutableList<Double> = mutableListOf(0.3, 0.3, 0.4)
    println(kullback_leibler_divergence(y_true_kl, y_pred_kl))
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
