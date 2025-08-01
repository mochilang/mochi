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

var x1: MutableList<Double> = mutableListOf(36.0, 40.0, 7.0, 39.0, 41.0, 15.0)
var x2: MutableList<Double> = mutableListOf(15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0)
var x3: MutableList<Double> = mutableListOf(0.14082834, 0.0974879, 1.73131507, 0.87636009, 0.0 - 1.95059594, 0.73438555, 0.0 - 0.03035726, 1.4667597, 0.0 - 0.74621349, 0.0 - 0.72588772, 0.6390516, 0.61501527, 0.0 - 0.9898378, 0.0 - 1.00447874, 0.0 - 0.62759469, 0.66206163, 1.04312009, 0.0 - 0.10305385, 0.75775634, 0.32566578)
fun sortFloat(xs: MutableList<Double>): MutableList<Double> {
    var arr: MutableList<Double> = xs
    var n: Int = arr.size
    var i: Int = 0
    while (i < n) {
        var j: Int = 0
        while (j < (n - 1)) {
            if (arr[j]!! > arr[j + 1]!!) {
                var t: Double = arr[j]!!
                (arr[j]) = arr[j + 1]!!
                (arr[j + 1]) = t
            }
            j = j + 1
        }
        i = i + 1
    }
    return arr
}

fun median(s: MutableList<Double>): Double {
    var n: Int = s.size
    if ((Math.floorMod(n, 2)) == 1) {
        return s[n / 2]!!
    }
    return (s[(n / 2) - 1]!! + s[n / 2]!!) / 2.0
}

fun fivenum(xs: MutableList<Double>): MutableList<Double> {
    var s: MutableList<Double> = sortFloat(xs)
    var n: Int = s.size
    var q1: Double = s[(n - 1) / 4]!!
    var med: Double = median(s)
    var q3: Double = s[(3 * (n - 1)) / 4]!!
    return mutableListOf(s[0]!!, q1, med, q3, s[n - 1]!!)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(fivenum(x1).toString())
        println(fivenum(x2).toString())
        println(fivenum(x3).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
