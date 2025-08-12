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

data class Dataset(var data: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>(), var target: MutableList<Double> = mutableListOf<Double>())
data class Tree(var threshold: Double = 0.0, var left_value: Double = 0.0, var right_value: Double = 0.0)
fun data_handling(dataset: Dataset): Dataset {
    return dataset
}

fun xgboost(features: MutableList<MutableList<Double>>, target: MutableList<Double>, test_features: MutableList<MutableList<Double>>): MutableList<Double> {
    var learning_rate: Double = 0.5
    var n_estimators: Int = (3).toInt()
    var trees: MutableList<Tree> = mutableListOf<Tree>()
    var predictions: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < target.size) {
        predictions = run { val _tmp = predictions.toMutableList(); _tmp.add(0.0); _tmp }
        i = i + 1
    }
    var est: Int = (0).toInt()
    while (est < n_estimators) {
        var residuals: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < target.size) {
            residuals = run { val _tmp = residuals.toMutableList(); _tmp.add(target[j]!! - predictions[j]!!); _tmp }
            j = j + 1
        }
        var sum_feat: Double = 0.0
        j = 0
        while (j < features.size) {
            sum_feat = sum_feat + (((features[j]!!) as MutableList<Double>))[0]!!
            j = j + 1
        }
        var threshold: Double = sum_feat / ((features.size.toDouble()))
        var left_sum: Double = 0.0
        var left_count: Int = (0).toInt()
        var right_sum: Double = 0.0
        var right_count: Int = (0).toInt()
        j = 0
        while (j < features.size) {
            if ((((features[j]!!) as MutableList<Double>))[0]!! <= threshold) {
                left_sum = left_sum + residuals[j]!!
                left_count = left_count + 1
            } else {
                right_sum = right_sum + residuals[j]!!
                right_count = right_count + 1
            }
            j = j + 1
        }
        var left_value: Double = 0.0
        if (left_count > 0) {
            left_value = left_sum / ((left_count.toDouble()))
        }
        var right_value: Double = 0.0
        if (right_count > 0) {
            right_value = right_sum / ((right_count.toDouble()))
        }
        j = 0
        while (j < features.size) {
            if ((((features[j]!!) as MutableList<Double>))[0]!! <= threshold) {
                _listSet(predictions, j, predictions[j]!! + (learning_rate * left_value))
            } else {
                _listSet(predictions, j, predictions[j]!! + (learning_rate * right_value))
            }
            j = j + 1
        }
        trees = run { val _tmp = trees.toMutableList(); _tmp.add(Tree(threshold = threshold, left_value = left_value, right_value = right_value)); _tmp }
        est = est + 1
    }
    var preds: MutableList<Double> = mutableListOf<Double>()
    var t: Int = (0).toInt()
    while (t < test_features.size) {
        var pred: Double = 0.0
        var k: Int = (0).toInt()
        while (k < trees.size) {
            if ((((test_features[t]!!) as MutableList<Double>))[0]!! <= trees[k]!!.threshold) {
                pred = pred + (learning_rate * trees[k]!!.left_value)
            } else {
                pred = pred + (learning_rate * trees[k]!!.right_value)
            }
            k = k + 1
        }
        preds = run { val _tmp = preds.toMutableList(); _tmp.add(pred); _tmp }
        t = t + 1
    }
    return preds
}

fun mean_absolute_error(y_true: MutableList<Double>, y_pred: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < y_true.size) {
        var diff: Double = y_true[i]!! - y_pred[i]!!
        if (diff < 0.0) {
            diff = 0.0 - diff
        }
        sum = sum + diff
        i = i + 1
    }
    return sum / ((y_true.size.toDouble()))
}

fun mean_squared_error(y_true: MutableList<Double>, y_pred: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < y_true.size) {
        var diff: Double = y_true[i]!! - y_pred[i]!!
        sum = sum + (diff * diff)
        i = i + 1
    }
    return sum / ((y_true.size.toDouble()))
}

fun user_main(): Unit {
    var california: Dataset = Dataset(data = mutableListOf(mutableListOf(1.0), mutableListOf(2.0), mutableListOf(3.0), mutableListOf(4.0)), target = mutableListOf(2.0, 3.0, 4.0, 5.0))
    var ds: Dataset = data_handling(california)
    var x_train: MutableList<MutableList<Double>> = ds.data
    var y_train: MutableList<Double> = ds.target
    var x_test: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(1.5), mutableListOf(3.5))
    var y_test: MutableList<Double> = mutableListOf(2.5, 4.5)
    var predictions: MutableList<Double> = xgboost(x_train, y_train, x_test)
    println("Predictions:")
    println(predictions)
    println("Mean Absolute Error:")
    println(mean_absolute_error(y_test, predictions))
    println("Mean Square Error:")
    println(mean_squared_error(y_test, predictions))
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
