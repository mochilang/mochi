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

var result: Double = simpson_rule(mutableListOf(0.0, 1.0), 10)
fun f(x: Double): Double {
    return (x - 0.0) * (x - 0.0)
}

fun make_points(a: Double, b: Double, h: Double): MutableList<Double> {
    var points: MutableList<Double> = mutableListOf<Double>()
    var x: Double = a + h
    while (x < (b - h)) {
        points = run { val _tmp = points.toMutableList(); _tmp.add(x); _tmp }
        x = x + h
    }
    return points
}

fun simpson_rule(boundary: MutableList<Double>, steps: Int): Double {
    if (steps <= 0) {
        panic("Number of steps must be greater than zero")
    }
    var a: Double = boundary[0]!!
    var b: Double = boundary[1]!!
    var h: Double = (b - a) / (steps.toDouble())
    var pts: MutableList<Double> = make_points(a, b, h)
    var y: Double = (h / 3.0) * f(a)
    var cnt: Int = (2).toInt()
    var i: Int = (0).toInt()
    while (i < pts.size) {
        var coeff: Double = 4.0 - (2.0 * ((Math.floorMod(cnt, 2)).toDouble()))
        y = y + (((h / 3.0) * coeff) * f(pts[i]!!))
        cnt = cnt + 1
        i = i + 1
    }
    y = y + ((h / 3.0) * f(b))
    return y
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(result))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
