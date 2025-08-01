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

var xs: MutableList<Double> = mutableListOf(0.0 - 0.5, 0.1, 0.5, 1.0, 1.5, 2.0, 3.0, 10.0, 140.0, 170.0)
fun ln(x: Double): Double {
    var k: Double = 0.0
    var v: Double = x
    while (v >= 2.0) {
        v = v / 2.0
        k = k + 1.0
    }
    while (v < 1.0) {
        v = v * 2.0
        k = k - 1.0
    }
    var z: Double = (v - 1.0) / (v + 1.0)
    var zpow: Double = z
    var sum: Double = z
    var i: Int = 3
    while (i <= 9) {
        zpow = (zpow * z) * z
        sum = sum + (zpow / i.toDouble())
        i = i + 2
    }
    var ln2: Double = 0.6931471805599453
    return (k * ln2) + (2.0 * sum)
}

fun expf(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var i: Int = 1
    while (i < 20) {
        term = (term * x) / i.toDouble()
        sum = sum + term
        i = i + 1
    }
    return sum
}

fun powf(base: Double, exp: Double): Double {
    return expf(exp * ln(base))
}

fun lanczos7(z: Double): Double {
    var t: Double = z + 6.5
    var x: Double = (((((((0.9999999999998099 + (676.5203681218851 / z)) - (1259.1392167224028 / (z + 1.0))) + (771.3234287776531 / (z + 2.0))) - (176.6150291621406 / (z + 3.0))) + (12.507343278686905 / (z + 4.0))) - (0.13857109526572012 / (z + 5.0))) + (0.000009984369578019572 / (z + 6.0))) + (0.00000015056327351493116 / (z + 7.0))
    return ((2.5066282746310002 * powf(t, z - 0.5)) * powf(2.718281828459045, 0.0 - t)) * x
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (x in xs) {
            println((x.toString() + " ") + lanczos7(x).toString())
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
