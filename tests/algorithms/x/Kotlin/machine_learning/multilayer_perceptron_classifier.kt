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

var X: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 0.0), mutableListOf(1.0, 1.0), mutableListOf(1.0, 0.0), mutableListOf(0.0, 1.0))
var Y: MutableList<Double> = mutableListOf(0.0, 1.0, 0.0, 0.0)
var test_data: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 0.0), mutableListOf(0.0, 1.0), mutableListOf(1.0, 1.0))
var w1: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.5, 0.0 - 0.5), mutableListOf(0.5, 0.5))
var b1: MutableList<Double> = mutableListOf(0.0, 0.0)
var w2: MutableList<Double> = mutableListOf(0.5, 0.0 - 0.5)
var b2: Double = 0.0
fun exp_taylor(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var i: Double = 1.0
    while (i < 20.0) {
        term = (term * x) / i
        sum = sum + term
        i = i + 1.0
    }
    return sum
}

fun sigmoid(x: Double): Double {
    return 1.0 / (1.0 + exp_taylor(0.0 - x))
}

fun train(epochs: Int, lr: Double): Unit {
    var e: Int = (0).toInt()
    while (e < epochs) {
        var i: Int = (0).toInt()
        while (i < X.size) {
            var x0: Double = (((X[i]!!) as MutableList<Double>))[0]!!
            var x1: Double = (((X[i]!!) as MutableList<Double>))[1]!!
            var target: Double = Y[i]!!
            var z1: Double = (((((w1[0]!!) as MutableList<Double>))[0]!! * x0) + ((((w1[1]!!) as MutableList<Double>))[0]!! * x1)) + b1[0]!!
            var z2: Double = (((((w1[0]!!) as MutableList<Double>))[1]!! * x0) + ((((w1[1]!!) as MutableList<Double>))[1]!! * x1)) + b1[1]!!
            var h1: Double = sigmoid(z1)
            var h2: Double = sigmoid(z2)
            var z3: Double = ((w2[0]!! * h1) + (w2[1]!! * h2)) + b2
            var out: Double = sigmoid(z3)
            var error: Double = out - target
            var d1: Double = ((h1 * (1.0 - h1)) * w2[0]!!) * error
            var d2: Double = ((h2 * (1.0 - h2)) * w2[1]!!) * error
            _listSet(w2, 0, w2[0]!! - ((lr * error) * h1))
            _listSet(w2, 1, w2[1]!! - ((lr * error) * h2))
            b2 = b2 - (lr * error)
            _listSet(w1[0]!!, 0, (((w1[0]!!) as MutableList<Double>))[0]!! - ((lr * d1) * x0))
            _listSet(w1[1]!!, 0, (((w1[1]!!) as MutableList<Double>))[0]!! - ((lr * d1) * x1))
            _listSet(b1, 0, b1[0]!! - (lr * d1))
            _listSet(w1[0]!!, 1, (((w1[0]!!) as MutableList<Double>))[1]!! - ((lr * d2) * x0))
            _listSet(w1[1]!!, 1, (((w1[1]!!) as MutableList<Double>))[1]!! - ((lr * d2) * x1))
            _listSet(b1, 1, b1[1]!! - (lr * d2))
            i = i + 1
        }
        e = e + 1
    }
}

fun predict(samples: MutableList<MutableList<Double>>): MutableList<Int> {
    var preds: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < samples.size) {
        var x0: Double = (((samples[i]!!) as MutableList<Double>))[0]!!
        var x1: Double = (((samples[i]!!) as MutableList<Double>))[1]!!
        var z1: Double = (((((w1[0]!!) as MutableList<Double>))[0]!! * x0) + ((((w1[1]!!) as MutableList<Double>))[0]!! * x1)) + b1[0]!!
        var z2: Double = (((((w1[0]!!) as MutableList<Double>))[1]!! * x0) + ((((w1[1]!!) as MutableList<Double>))[1]!! * x1)) + b1[1]!!
        var h1: Double = sigmoid(z1)
        var h2: Double = sigmoid(z2)
        var z3: Double = ((w2[0]!! * h1) + (w2[1]!! * h2)) + b2
        var out: Double = sigmoid(z3)
        var label: Int = (0).toInt()
        if (out >= 0.5) {
            label = 1
        }
        preds = run { val _tmp = preds.toMutableList(); _tmp.add(label); _tmp }
        i = i + 1
    }
    return preds
}

fun wrapper(y: MutableList<Int>): MutableList<Int> {
    return y
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        train(4000, 0.5)
        var preds: MutableList<Int> = wrapper(predict(test_data))
        println(preds.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
