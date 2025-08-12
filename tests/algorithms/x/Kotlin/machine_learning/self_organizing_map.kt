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

fun get_winner(weights: MutableList<MutableList<Double>>, sample: MutableList<Int>): Int {
    var d0: Double = 0.0
    var d1: Double = 0.0
    for (i in 0 until sample.size) {
        var diff0: Double = sample[i]!! - (((weights[0]!!) as MutableList<Double>))[i]!!
        var diff1: Double = sample[i]!! - (((weights[1]!!) as MutableList<Double>))[i]!!
        d0 = d0 + (diff0 * diff0)
        d1 = d1 + (diff1 * diff1)
        return (if (d0 > d1) 0 else 1 as Int)
    }
    return 0
}

fun update(weights: MutableList<MutableList<Double>>, sample: MutableList<Int>, j: Int, alpha: Double): MutableList<MutableList<Double>> {
    for (i in 0 until weights.size) {
        _listSet(weights[j]!!, i, (((weights[j]!!) as MutableList<Double>))[i]!! + (alpha * (sample[i]!! - (((weights[j]!!) as MutableList<Double>))[i]!!)))
    }
    return weights
}

fun list_to_string(xs: MutableList<Double>): String {
    var s: String = "["
    var i: Int = (0).toInt()
    while (i < xs.size) {
        s = s + (xs[i]!!).toString()
        if (i < (xs.size - 1)) {
            s = s + ", "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun matrix_to_string(m: MutableList<MutableList<Double>>): String {
    var s: String = "["
    var i: Int = (0).toInt()
    while (i < m.size) {
        s = s + list_to_string(m[i]!!)
        if (i < (m.size - 1)) {
            s = s + ", "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun user_main(): Unit {
    var training_samples: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 1, 0, 0), mutableListOf(0, 0, 0, 1), mutableListOf(1, 0, 0, 0), mutableListOf(0, 0, 1, 1))
    var weights: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.2, 0.6, 0.5, 0.9), mutableListOf(0.8, 0.4, 0.7, 0.3))
    var epochs: Int = (3).toInt()
    var alpha: Double = 0.5
    for (_u1 in 0 until epochs) {
        for (j in 0 until training_samples.size) {
            var sample: MutableList<Int> = training_samples[j]!!
            var winner: Int = (get_winner(weights, sample)).toInt()
            weights = update(weights, sample, winner, alpha)
        }
    }
    var sample: MutableList<Int> = mutableListOf(0, 0, 0, 1)
    var winner: Int = (get_winner(weights, sample)).toInt()
    println("Clusters that the test sample belongs to : " + winner.toString())
    println("Weights that have been trained : " + matrix_to_string(weights))
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
