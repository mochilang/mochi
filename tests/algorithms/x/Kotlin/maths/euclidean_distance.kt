fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

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

fun sqrtApprox(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun euclidean_distance(v1: MutableList<Double>, v2: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < v1.size) {
        var diff: Double = v1[i]!! - v2[i]!!
        sum = sum + (diff * diff)
        i = i + 1
    }
    return sqrtApprox(sum)
}

fun euclidean_distance_no_np(v1: MutableList<Double>, v2: MutableList<Double>): Double {
    return euclidean_distance(v1, v2)
}

fun user_main(): Unit {
    println(_numToStr(euclidean_distance(mutableListOf(0.0, 0.0), mutableListOf(2.0, 2.0))))
    println(_numToStr(euclidean_distance(mutableListOf(0.0, 0.0, 0.0), mutableListOf(2.0, 2.0, 2.0))))
    println(_numToStr(euclidean_distance(mutableListOf(1.0, 2.0, 3.0, 4.0), mutableListOf(5.0, 6.0, 7.0, 8.0))))
    println(_numToStr(euclidean_distance_no_np(mutableListOf(1.0, 2.0, 3.0, 4.0), mutableListOf(5.0, 6.0, 7.0, 8.0))))
    println(_numToStr(euclidean_distance_no_np(mutableListOf(0.0, 0.0), mutableListOf(2.0, 2.0))))
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
