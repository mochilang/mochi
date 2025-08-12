import java.math.BigInteger

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

var samples: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(2.0, 2.0), mutableListOf(1.5, 1.5), mutableListOf(0.0, 0.0), mutableListOf(0.5, 0.0))
var labels: MutableList<Double> = mutableListOf(1.0, 1.0, 0.0 - 1.0, 0.0 - 1.0)
var model: MutableList<MutableList<Double>> = smo_train(samples, labels, 1.0, 0.001, 10)
fun dot(a: MutableList<Double>, b: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < a.size) {
        sum = sum + (a[i]!! * b[i]!!)
        i = i + 1
    }
    return sum
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

fun absf(x: Double): Double {
    if (x >= 0.0) {
        return x
    }
    return 0.0 - x
}

fun predict_raw(samples: MutableList<MutableList<Double>>, labels: MutableList<Double>, alphas: MutableList<Double>, b: Double, x: MutableList<Double>): Double {
    var res: Double = 0.0
    var i: Int = (0).toInt()
    while (i < samples.size) {
        res = res + ((alphas[i]!! * labels[i]!!) * dot(samples[i]!!, x))
        i = i + 1
    }
    return res + b
}

fun smo_train(samples: MutableList<MutableList<Double>>, labels: MutableList<Double>, c: Double, tol: Double, max_passes: Int): MutableList<MutableList<Double>> {
    var m: Int = (samples.size).toInt()
    var alphas: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < m) {
        alphas = run { val _tmp = alphas.toMutableList(); _tmp.add(0.0); _tmp }
        i = i + 1
    }
    var b: Double = 0.0
    var passes: Int = (0).toInt()
    while (passes < max_passes) {
        var num_changed: Int = (0).toInt()
        var i1: Int = (0).toInt()
        while (i1 < m) {
            var Ei: Double = predict_raw(samples, labels, alphas, b, samples[i1]!!) - labels[i1]!!
            if (((((labels[i1]!! * Ei) < (0.0 - tol)) && (alphas[i1]!! < c) as Boolean)) || ((((labels[i1]!! * Ei) > tol) && (alphas[i1]!! > 0.0) as Boolean))) {
                var i2: BigInteger = ((Math.floorMod((i1 + 1), m)).toBigInteger())
                var Ej: Double = predict_raw(samples, labels, alphas, b, samples[(i2).toInt()]!!) - labels[(i2).toInt()]!!
                var alpha1_old: Double = alphas[i1]!!
                var alpha2_old: Double = alphas[(i2).toInt()]!!
                var L: Double = 0.0
                var H: Double = 0.0
                if (labels[i1]!! != labels[(i2).toInt()]!!) {
                    L = maxf(0.0, alpha2_old - alpha1_old)
                    H = minf(c, (c + alpha2_old) - alpha1_old)
                } else {
                    L = maxf(0.0, (alpha2_old + alpha1_old) - c)
                    H = minf(c, alpha2_old + alpha1_old)
                }
                if (L == H) {
                    i1 = i1 + 1
                    continue
                }
                var eta: Double = ((2.0 * dot(samples[i1]!!, samples[(i2).toInt()]!!)) - dot(samples[i1]!!, samples[i1]!!)) - dot(samples[(i2).toInt()]!!, samples[(i2).toInt()]!!)
                if (eta >= 0.0) {
                    i1 = i1 + 1
                    continue
                }
                _listSet(alphas, (i2).toInt(), alpha2_old - ((labels[(i2).toInt()]!! * (Ei - Ej)) / eta))
                if (alphas[(i2).toInt()]!! > H) {
                    _listSet(alphas, (i2).toInt(), H)
                }
                if (alphas[(i2).toInt()]!! < L) {
                    _listSet(alphas, (i2).toInt(), L)
                }
                if (absf(alphas[(i2).toInt()]!! - alpha2_old) < 0.00001) {
                    i1 = i1 + 1
                    continue
                }
                _listSet(alphas, i1, alpha1_old + ((labels[i1]!! * labels[(i2).toInt()]!!) * (alpha2_old - alphas[(i2).toInt()]!!)))
                var b1: Double = ((b - Ei) - ((labels[i1]!! * (alphas[i1]!! - alpha1_old)) * dot(samples[i1]!!, samples[i1]!!))) - ((labels[(i2).toInt()]!! * (alphas[(i2).toInt()]!! - alpha2_old)) * dot(samples[i1]!!, samples[(i2).toInt()]!!))
                var b2: Double = ((b - Ej) - ((labels[i1]!! * (alphas[i1]!! - alpha1_old)) * dot(samples[i1]!!, samples[(i2).toInt()]!!))) - ((labels[(i2).toInt()]!! * (alphas[(i2).toInt()]!! - alpha2_old)) * dot(samples[(i2).toInt()]!!, samples[(i2).toInt()]!!))
                if ((alphas[i1]!! > 0.0) && (alphas[i1]!! < c)) {
                    b = b1
                } else {
                    if ((alphas[(i2).toInt()]!! > 0.0) && (alphas[(i2).toInt()]!! < c)) {
                        b = b2
                    } else {
                        b = (b1 + b2) / 2.0
                    }
                }
                num_changed = num_changed + 1
            }
            i1 = i1 + 1
        }
        if (num_changed == 0) {
            passes = passes + 1
        } else {
            passes = 0
        }
    }
    return mutableListOf(alphas, mutableListOf(b))
}

fun predict(samples: MutableList<MutableList<Double>>, labels: MutableList<Double>, model: MutableList<MutableList<Double>>, x: MutableList<Double>): Double {
    var alphas: MutableList<Double> = model[0]!!
    var b: Double = (((model[1]!!) as MutableList<Double>))[0]!!
    var _val: Double = predict_raw(samples, labels, alphas, b, x)
    if (_val >= 0.0) {
        return 1.0
    }
    return 0.0 - 1.0
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(predict(samples, labels, model, mutableListOf(1.5, 1.0)))
        println(predict(samples, labels, model, mutableListOf(0.2, 0.1)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
