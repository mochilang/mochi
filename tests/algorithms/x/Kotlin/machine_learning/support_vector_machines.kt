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

data class SVC(var weights: MutableList<Double> = mutableListOf<Double>(), var bias: Double = 0.0, var lr: Double = 0.0, var lambda: Double = 0.0, var epochs: Int = 0)
var xs: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 1.0), mutableListOf(0.0, 2.0), mutableListOf(1.0, 1.0), mutableListOf(1.0, 2.0))
var ys: MutableList<Int> = mutableListOf(1, 1, 0 - 1, 0 - 1)
var base: SVC = new_svc(0.01, 0.01, 1000)
var model: SVC = fit(base, xs, ys)
fun dot(a: MutableList<Double>, b: MutableList<Double>): Double {
    var s: Double = 0.0
    var i: Int = (0).toInt()
    while (i < a.size) {
        s = s + (a[i]!! * b[i]!!)
        i = i + 1
    }
    return s
}

fun new_svc(lr: Double, lambda: Double, epochs: Int): SVC {
    return SVC(weights = mutableListOf<Double>(), bias = 0.0, lr = lr, lambda = lambda, epochs = epochs)
}

fun fit(model: SVC, xs: MutableList<MutableList<Double>>, ys: MutableList<Int>): SVC {
    var n_features: Int = ((xs[0]!!).size).toInt()
    var w: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < n_features) {
        w = run { val _tmp = w.toMutableList(); _tmp.add(0.0); _tmp }
        i = i + 1
    }
    var b: Double = 0.0
    var epoch: Int = (0).toInt()
    while (epoch < model.epochs) {
        var j: Int = (0).toInt()
        while (j < xs.size) {
            var x: MutableList<Double> = xs[j]!!
            var y: Double = ((ys[j]!!).toDouble())
            var prod: Double = dot(w, x) + b
            if ((y * prod) < 1.0) {
                var k: Int = (0).toInt()
                while (k < w.size) {
                    _listSet(w, k, w[k]!! + (model.lr * ((y * x[k]!!) - ((2.0 * model.lambda) * w[k]!!))))
                    k = k + 1
                }
                b = b + (model.lr * y)
            } else {
                var k: Int = (0).toInt()
                while (k < w.size) {
                    _listSet(w, k, w[k]!! - (model.lr * ((2.0 * model.lambda) * w[k]!!)))
                    k = k + 1
                }
            }
            j = j + 1
        }
        epoch = epoch + 1
    }
    return SVC(weights = w, bias = b, lr = model.lr, lambda = model.lambda, epochs = model.epochs)
}

fun predict(model: SVC, x: MutableList<Double>): Int {
    var s: Double = dot(model.weights, x) + model.bias
    if (s >= 0.0) {
        return 1
    } else {
        return 0 - 1
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(predict(model, mutableListOf(0.0, 1.0)))
        println(predict(model, mutableListOf(1.0, 1.0)))
        println(predict(model, mutableListOf(2.0, 2.0)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
