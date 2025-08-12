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

var x: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.5, 1.5), mutableListOf(1.0, 1.0), mutableListOf(1.5, 0.5), mutableListOf(3.0, 3.5), mutableListOf(3.5, 3.0), mutableListOf(4.0, 4.0))
var y: MutableList<Double> = mutableListOf(0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
var alpha: Double = 0.1
var iterations: Int = (1000).toInt()
var theta: MutableList<Double> = logistic_reg(alpha, x, y, iterations)
fun expApprox(x: Double): Double {
    var y: Double = x
    var is_neg: Boolean = false
    if (x < 0.0) {
        is_neg = true
        y = 0.0 - x
    }
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = (1).toInt()
    while (n < 30) {
        term = (term * y) / ((n.toDouble()))
        sum = sum + term
        n = n + 1
    }
    if ((is_neg as Boolean)) {
        return 1.0 / sum
    }
    return sum
}

fun sigmoid(z: Double): Double {
    return 1.0 / (1.0 + expApprox(0.0 - z))
}

fun dot(a: MutableList<Double>, b: MutableList<Double>): Double {
    var s: Double = 0.0
    var i: Int = (0).toInt()
    while (i < a.size) {
        s = s + (a[i]!! * b[i]!!)
        i = i + 1
    }
    return s
}

fun zeros(n: Int): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < n) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(0.0); _tmp }
        i = i + 1
    }
    return res
}

fun logistic_reg(alpha: Double, x: MutableList<MutableList<Double>>, y: MutableList<Double>, iterations: Int): MutableList<Double> {
    var m: Int = (x.size).toInt()
    var n: Int = ((x[0]!!).size).toInt()
    var theta: MutableList<Double> = zeros(n)
    var iter: Int = (0).toInt()
    while (iter < iterations) {
        var grad: MutableList<Double> = zeros(n)
        var i: Int = (0).toInt()
        while (i < m) {
            var z: Double = dot(x[i]!!, theta)
            var h: Double = sigmoid(z)
            var k: Int = (0).toInt()
            while (k < n) {
                _listSet(grad, k, grad[k]!! + ((h - y[i]!!) * (((x[i]!!) as MutableList<Double>))[k]!!))
                k = k + 1
            }
            i = i + 1
        }
        var k2: Int = (0).toInt()
        while (k2 < n) {
            _listSet(theta, k2, theta[k2]!! - ((alpha * grad[k2]!!) / ((m.toDouble()))))
            k2 = k2 + 1
        }
        iter = iter + 1
    }
    return theta
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (i in 0 until theta.size) {
            println(theta[i]!!)
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
