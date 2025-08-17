fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

var x_points: MutableList<Double> = mutableListOf(0.0, 1.0, 2.0, 3.0)
var y_points: MutableList<Double> = mutableListOf(0.0, 1.0, 8.0, 27.0)
fun ucal(u: Double, p: Int): Double {
    var temp: Double = u
    var i: Int = (1).toInt()
    while (i < p) {
        temp = temp * (u - (i.toDouble()))
        i = i + 1
    }
    return temp
}

fun factorial(n: Int): Double {
    var result: Double = 1.0
    var i: Int = (2).toInt()
    while (i <= n) {
        result = result * (i.toDouble())
        i = i + 1
    }
    return result
}

fun newton_forward_interpolation(x: MutableList<Double>, y0: MutableList<Double>, value: Double): Double {
    var n: Int = (x.size).toInt()
    var y: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < n) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < n) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0.0); _tmp }
            j = j + 1
        }
        y = run { val _tmp = y.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    i = 0
    while (i < n) {
        _listSet(y[i]!!, 0, y0[i]!!)
        i = i + 1
    }
    var i1: Int = (1).toInt()
    while (i1 < n) {
        var j1: Int = (0).toInt()
        while (j1 < (n - i1)) {
            _listSet(y[j1]!!, i1, ((y[j1 + 1]!!) as MutableList<Double>)[i1 - 1]!! - ((y[j1]!!) as MutableList<Double>)[i1 - 1]!!)
            j1 = j1 + 1
        }
        i1 = i1 + 1
    }
    var u: Double = (value - x[0]!!) / (x[1]!! - x[0]!!)
    var sum: Double = ((y[0]!!) as MutableList<Double>)[0]!!
    var k: Int = (1).toInt()
    while (k < n) {
        sum = sum + ((ucal(u, k) * ((y[0]!!) as MutableList<Double>)[k]!!) / factorial(k))
        k = k + 1
    }
    return sum
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(newton_forward_interpolation(x_points, y_points, 1.5)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
