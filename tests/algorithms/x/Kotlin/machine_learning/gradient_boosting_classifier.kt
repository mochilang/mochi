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

data class Stump(var feature: Int = 0, var threshold: Double = 0.0, var left: Double = 0.0, var right: Double = 0.0)
var features: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(1.0), mutableListOf(2.0), mutableListOf(3.0), mutableListOf(4.0))
var target: MutableList<Double> = mutableListOf(0.0 - 1.0, 0.0 - 1.0, 1.0, 1.0)
var models: MutableList<Stump> = fit(5, 0.5, features, target)
var predictions: MutableList<Double> = predict(models, features, 0.5)
var acc: Double = accuracy(predictions, target)
fun exp_approx(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var i: Int = (1).toInt()
    while (i < 10) {
        term = (term * x) / ((i.toDouble()))
        sum = sum + term
        i = i + 1
    }
    return sum
}

fun signf(x: Double): Double {
    if (x >= 0.0) {
        return 1.0
    }
    return 0.0 - 1.0
}

fun gradient(target: MutableList<Double>, preds: MutableList<Double>): MutableList<Double> {
    var n: Int = (target.size).toInt()
    var residuals: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < n) {
        var t: Double = target[i]!!
        var y: Double = preds[i]!!
        var exp_val: Double = exp_approx(t * y)
        var res: Double = (0.0 - t) / (1.0 + exp_val)
        residuals = run { val _tmp = residuals.toMutableList(); _tmp.add(res); _tmp }
        i = i + 1
    }
    return residuals
}

fun predict_raw(models: MutableList<Stump>, features: MutableList<MutableList<Double>>, learning_rate: Double): MutableList<Double> {
    var n: Int = (features.size).toInt()
    var preds: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < n) {
        preds = run { val _tmp = preds.toMutableList(); _tmp.add(0.0); _tmp }
        i = i + 1
    }
    var m: Int = (0).toInt()
    while (m < models.size) {
        var stump: Stump = models[m]!!
        i = 0
        while (i < n) {
            var value: Double = (((features[i]!!) as MutableList<Double>))[stump.feature]!!
            if (value <= stump.threshold) {
                _listSet(preds, i, preds[i]!! + (learning_rate * stump.left))
            } else {
                _listSet(preds, i, preds[i]!! + (learning_rate * stump.right))
            }
            i = i + 1
        }
        m = m + 1
    }
    return preds
}

fun predict(models: MutableList<Stump>, features: MutableList<MutableList<Double>>, learning_rate: Double): MutableList<Double> {
    var raw: MutableList<Double> = predict_raw(models, features, learning_rate)
    var result: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < raw.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(signf(raw[i]!!)); _tmp }
        i = i + 1
    }
    return result
}

fun train_stump(features: MutableList<MutableList<Double>>, residuals: MutableList<Double>): Stump {
    var n_samples: Int = (features.size).toInt()
    var n_features: Int = ((features[0]!!).size).toInt()
    var best_feature: Int = (0).toInt()
    var best_threshold: Double = 0.0
    var best_error: Double = 1000000000.0
    var best_left: Double = 0.0
    var best_right: Double = 0.0
    var j: Int = (0).toInt()
    while (j < n_features) {
        var t_index: Int = (0).toInt()
        while (t_index < n_samples) {
            var t: Double = (((features[t_index]!!) as MutableList<Double>))[j]!!
            var sum_left: Double = 0.0
            var count_left: Int = (0).toInt()
            var sum_right: Double = 0.0
            var count_right: Int = (0).toInt()
            var i: Int = (0).toInt()
            while (i < n_samples) {
                if ((((features[i]!!) as MutableList<Double>))[j]!! <= t) {
                    sum_left = sum_left + residuals[i]!!
                    count_left = count_left + 1
                } else {
                    sum_right = sum_right + residuals[i]!!
                    count_right = count_right + 1
                }
                i = i + 1
            }
            var left_val: Double = 0.0
            if (count_left != 0) {
                left_val = sum_left / ((count_left.toDouble()))
            }
            var right_val: Double = 0.0
            if (count_right != 0) {
                right_val = sum_right / ((count_right.toDouble()))
            }
            var error: Double = 0.0
            i = 0
            while (i < n_samples) {
                var pred: Double = (if ((((features[i]!!) as MutableList<Double>))[j]!! <= t) left_val else right_val.toDouble())
                var diff: Double = residuals[i]!! - pred
                error = error + (diff * diff)
                i = i + 1
            }
            if (error < best_error) {
                best_error = error
                best_feature = j
                best_threshold = t
                best_left = left_val
                best_right = right_val
            }
            t_index = t_index + 1
        }
        j = j + 1
    }
    return Stump(feature = best_feature, threshold = best_threshold, left = best_left, right = best_right)
}

fun fit(n_estimators: Int, learning_rate: Double, features: MutableList<MutableList<Double>>, target: MutableList<Double>): MutableList<Stump> {
    var models: MutableList<Stump> = mutableListOf<Stump>()
    var m: Int = (0).toInt()
    while (m < n_estimators) {
        var preds: MutableList<Double> = predict_raw(models, features, learning_rate)
        var grad: MutableList<Double> = gradient(target, preds)
        var residuals: MutableList<Double> = mutableListOf<Double>()
        var i: Int = (0).toInt()
        while (i < grad.size) {
            residuals = run { val _tmp = residuals.toMutableList(); _tmp.add(0.0 - grad[i]!!); _tmp }
            i = i + 1
        }
        var stump: Stump = train_stump(features, residuals)
        models = run { val _tmp = models.toMutableList(); _tmp.add(stump); _tmp }
        m = m + 1
    }
    return models
}

fun accuracy(preds: MutableList<Double>, target: MutableList<Double>): Double {
    var n: Int = (target.size).toInt()
    var correct: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        if (preds[i]!! == target[i]!!) {
            correct = correct + 1
        }
        i = i + 1
    }
    return ((correct.toDouble())) / ((n.toDouble()))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Accuracy: " + acc.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
