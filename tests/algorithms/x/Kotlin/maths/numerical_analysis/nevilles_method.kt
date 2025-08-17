fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

data class NevilleResult(var value: Double = 0.0, var table: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>())
fun neville_interpolate(x_points: MutableList<Double>, y_points: MutableList<Double>, x0: Double): NevilleResult {
    var n: Int = (x_points.size).toInt()
    var q: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < n) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < n) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0.0); _tmp }
            j = j + 1
        }
        q = run { val _tmp = q.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    i = 0
    while (i < n) {
        _listSet(q[i]!!, 1, y_points[i]!!)
        i = i + 1
    }
    var col: Int = (2).toInt()
    while (col < n) {
        var row_idx: Int = (col).toInt()
        while (row_idx < n) {
            _listSet(q[row_idx]!!, col, (((x0 - x_points[(row_idx - col) + 1]!!) * ((q[row_idx]!!) as MutableList<Double>)[col - 1]!!) - ((x0 - x_points[row_idx]!!) * ((q[row_idx - 1]!!) as MutableList<Double>)[col - 1]!!)) / (x_points[row_idx]!! - x_points[(row_idx - col) + 1]!!))
            row_idx = row_idx + 1
        }
        col = col + 1
    }
    return NevilleResult(value = ((q[n - 1]!!) as MutableList<Double>)[n - 1]!!, table = q)
}

fun test_neville(): Unit {
    var xs: MutableList<Double> = mutableListOf(1.0, 2.0, 3.0, 4.0, 6.0)
    var ys: MutableList<Double> = mutableListOf(6.0, 7.0, 8.0, 9.0, 11.0)
    var r1: NevilleResult = neville_interpolate(xs, ys, 5.0)
    if (r1.value != 10.0) {
        panic("neville_interpolate at 5 failed")
    }
    var r2: NevilleResult = neville_interpolate(xs, ys, 99.0)
    if (r2.value != 104.0) {
        panic("neville_interpolate at 99 failed")
    }
}

fun user_main(): Unit {
    test_neville()
    var xs: MutableList<Double> = mutableListOf(1.0, 2.0, 3.0, 4.0, 6.0)
    var ys: MutableList<Double> = mutableListOf(6.0, 7.0, 8.0, 9.0, 11.0)
    var r: NevilleResult = neville_interpolate(xs, ys, 5.0)
    println(r.value)
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
