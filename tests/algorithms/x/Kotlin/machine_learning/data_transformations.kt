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

fun floor(x: Double): Double {
    var i: Int = ((x.toInt())).toInt()
    if (((i.toDouble())) > x) {
        i = i - 1
    }
    return (i.toDouble())
}

fun pow10(n: Int): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < n) {
        result = result * 10.0
        i = i + 1
    }
    return result
}

fun round(x: Double, n: Int): Double {
    var m: Double = pow10(n)
    var y: Double = ((floor((x * m) + 0.5)).toDouble())
    return y / m
}

fun sqrtApprox(x: Double): Double {
    var guess: Double = x
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun mean(data: MutableList<Double>): Double {
    var total: Double = 0.0
    var i: Int = (0).toInt()
    var n: Int = (data.size).toInt()
    while (i < n) {
        total = total + data[i]!!
        i = i + 1
    }
    return total / ((n.toDouble()))
}

fun stdev(data: MutableList<Double>): Double {
    var n: Int = (data.size).toInt()
    if (n <= 1) {
        panic("data length must be > 1")
    }
    var m: Double = mean(data)
    var sum_sq: Double = 0.0
    var i: Int = (0).toInt()
    while (i < n) {
        var diff: Double = data[i]!! - m
        sum_sq = sum_sq + (diff * diff)
        i = i + 1
    }
    return sqrtApprox(sum_sq / (((n - 1).toDouble())))
}

fun normalization(data: MutableList<Double>, ndigits: Int): MutableList<Double> {
    var x_min: Double = (data.min()!!.toDouble())
    var x_max: Double = (data.max()!!.toDouble())
    var denom: Double = x_max - x_min
    var result: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    var n: Int = (data.size).toInt()
    while (i < n) {
        var norm: Double = (data[i]!! - x_min) / denom
        result = run { val _tmp = result.toMutableList(); _tmp.add(round(norm, ndigits)); _tmp }
        i = i + 1
    }
    return result
}

fun standardization(data: MutableList<Double>, ndigits: Int): MutableList<Double> {
    var mu: Double = mean(data)
    var sigma: Double = stdev(data)
    var result: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    var n: Int = (data.size).toInt()
    while (i < n) {
        var z: Double = (data[i]!! - mu) / sigma
        result = run { val _tmp = result.toMutableList(); _tmp.add(round(z, ndigits)); _tmp }
        i = i + 1
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(normalization(mutableListOf(2.0, 7.0, 10.0, 20.0, 30.0, 50.0), 3).toString())
        println(normalization(mutableListOf(5.0, 10.0, 15.0, 20.0, 25.0), 3).toString())
        println(standardization(mutableListOf(2.0, 7.0, 10.0, 20.0, 30.0, 50.0), 3).toString())
        println(standardization(mutableListOf(5.0, 10.0, 15.0, 20.0, 25.0), 3).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
