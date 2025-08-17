fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

fun runge_kutta(f: (Double, Double) -> Double, y0: Double, x0: Double, h: Double, x_end: Double): MutableList<Double> {
    var span: Double = (x_end - x0) / h
    var n: Int = (span.toInt()).toInt()
    if ((n.toDouble()) < span) {
        n = n + 1
    }
    var y: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < (n + 1)) {
        y = run { val _tmp = y.toMutableList(); _tmp.add(0.0); _tmp }
        i = i + 1
    }
    _listSet(y, 0, y0)
    var x: Double = x0
    var k: Int = (0).toInt()
    while (k < n) {
        var k1: Double = (f(x, y[k]!!)).toDouble()
        var k2: Double = (f(x + (0.5 * h), y[k]!! + ((0.5 * h) * k1))).toDouble()
        var k3: Double = (f(x + (0.5 * h), y[k]!! + ((0.5 * h) * k2))).toDouble()
        var k4: Double = (f(x + h, y[k]!! + (h * k3))).toDouble()
        _listSet(y, k + 1, y[k]!! + (((1.0 / 6.0) * h) * (((k1 + (2.0 * k2)) + (2.0 * k3)) + k4)))
        x = x + h
        k = k + 1
    }
    return y
}

fun test_runge_kutta(): Unit {
    fun f(x: Double, y: Double): Double {
        return y
    }

    var result: MutableList<Double> = runge_kutta(::f, 1.0, 0.0, 0.01, 5.0)
    var last: Double = result[result.size - 1]!!
    var expected: Double = 148.41315904125113
    var diff: Double = last - expected
    if (diff < 0.0) {
        diff = 0.0 - diff
    }
    if (diff > 0.000001) {
        panic("runge_kutta failed")
    }
}

fun user_main(): Unit {
    test_runge_kutta()
    fun f(x: Double, y: Double): Double {
        return y
    }

    var r: MutableList<Double> = runge_kutta(::f, 1.0, 0.0, 0.1, 1.0)
    println(_numToStr(r[r.size - 1]!!))
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
