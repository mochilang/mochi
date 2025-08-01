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

data class Point(var x: Double, var y: Double)
data class Line(var slope: Double, var yint: Double)
fun createLine(a: Point, b: Point): Line {
    var slope: Double = (b.y - a.y) / (b.x - a.x)
    var yint: Double = a.y - (slope * a.x)
    return Line(slope = slope, yint = yint)
}

fun evalX(l: Line, x: Double): Double {
    return (l.slope * x) + l.yint
}

fun intersection(l1: Line, l2: Line): Point {
    if (l1.slope == l2.slope) {
        return Point(x = 0.0, y = 0.0)
    }
    var x: Double = (l2.yint - l1.yint) / (l1.slope - l2.slope)
    var y: Double = evalX(l1, x)
    return Point(x = x, y = y)
}

fun user_main(): Unit {
    var l1: Line = createLine(Point(x = 4.0, y = 0.0), Point(x = 6.0, y = 10.0))
    var l2: Line = createLine(Point(x = 0.0, y = 3.0), Point(x = 10.0, y = 7.0))
    var p: Point = intersection(l1, l2)
    println(((("{" + p.x.toString()) + " ") + p.y.toString()) + "}")
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
