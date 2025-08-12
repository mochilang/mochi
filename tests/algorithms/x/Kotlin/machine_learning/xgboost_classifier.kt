fun <T> concat(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    res.addAll(a)
    res.addAll(b)
    return res
}

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
fun mean(xs: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < xs.size) {
        sum = sum + xs[i]!!
        i = i + 1
    }
    return sum / (xs.size * 1.0)
}

fun stump_predict(s: Stump, x: MutableList<Double>): Double {
    if (x[s.feature]!! < s.threshold) {
        return s.left
    }
    return s.right
}

fun train_stump(features: MutableList<MutableList<Double>>, residuals: MutableList<Double>): Stump {
    var best_feature: Int = (0).toInt()
    var best_threshold: Double = 0.0
    var best_error: Double = 1000000000.0
    var best_left: Double = 0.0
    var best_right: Double = 0.0
    var num_features: Int = ((features[0]!!).size).toInt()
    var f: Int = (0).toInt()
    while (f < num_features) {
        var i: Int = (0).toInt()
        while (i < features.size) {
            var threshold: Double = (((features[i]!!) as MutableList<Double>))[f]!!
            var left: MutableList<Double> = mutableListOf<Double>()
            var right: MutableList<Double> = mutableListOf<Double>()
            var j: Int = (0).toInt()
            while (j < features.size) {
                if ((((features[j]!!) as MutableList<Double>))[f]!! < threshold) {
                    left = ((concat(left, mutableListOf(residuals[j]!!))) as MutableList<Double>)
                } else {
                    right = ((concat(right, mutableListOf(residuals[j]!!))) as MutableList<Double>)
                }
                j = j + 1
            }
            if ((left.size != 0) && (right.size != 0)) {
                var left_mean: Double = mean(left)
                var right_mean: Double = mean(right)
                var err: Double = 0.0
                j = 0
                while (j < features.size) {
                    var pred: Double = (if ((((features[j]!!) as MutableList<Double>))[f]!! < threshold) left_mean else right_mean.toDouble())
                    var diff: Double = residuals[j]!! - pred
                    err = err + (diff * diff)
                    j = j + 1
                }
                if (err < best_error) {
                    best_error = err
                    best_feature = f
                    best_threshold = threshold
                    best_left = left_mean
                    best_right = right_mean
                }
            }
            i = i + 1
        }
        f = f + 1
    }
    return Stump(feature = best_feature, threshold = best_threshold, left = best_left, right = best_right)
}

fun boost(features: MutableList<MutableList<Double>>, targets: MutableList<Int>, rounds: Int): MutableList<Stump> {
    var model: MutableList<Stump> = mutableListOf<Stump>()
    var preds: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < targets.size) {
        preds = ((concat(preds, mutableListOf(0.0))) as MutableList<Double>)
        i = i + 1
    }
    var r: Int = (0).toInt()
    while (r < rounds) {
        var residuals: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < targets.size) {
            residuals = ((concat(residuals, mutableListOf(targets[j]!! - preds[j]!!))) as MutableList<Double>)
            j = j + 1
        }
        var stump: Stump = train_stump(features, residuals)
        model = ((concat(model, mutableListOf(stump))) as MutableList<Stump>)
        j = 0
        while (j < preds.size) {
            _listSet(preds, j, preds[j]!! + stump_predict(stump, features[j]!!))
            j = j + 1
        }
        r = r + 1
    }
    return model
}

fun predict(model: MutableList<Stump>, x: MutableList<Double>): Double {
    var score: Double = 0.0
    var i: Int = (0).toInt()
    while (i < model.size) {
        var s: Stump = model[i]!!
        if (x[s.feature]!! < s.threshold) {
            score = score + s.left
        } else {
            score = score + s.right
        }
        i = i + 1
    }
    return score
}

fun user_main(): Unit {
    var features: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(5.1, 3.5), mutableListOf(4.9, 3.0), mutableListOf(6.2, 3.4), mutableListOf(5.9, 3.0))
    var targets: MutableList<Int> = mutableListOf(0, 0, 1, 1)
    var model: MutableList<Stump> = boost(features, targets, 3)
    var out: String = ""
    var i: Int = (0).toInt()
    while (i < features.size) {
        var s: Double = predict(model, features[i]!!)
        var label: Int = (if (s >= 0.5) 1 else 0).toInt()
        if (i == 0) {
            out = label.toString()
        } else {
            out = (out + " ") + label.toString()
        }
        i = i + 1
    }
    println(out)
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
