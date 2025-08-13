import java.math.BigInteger

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

data class Point(var x: Int = 0, var y: Int = 0)
fun abs_int(n: Int): Int {
    if (n < 0) {
        return 0 - n
    }
    return n
}

fun round_int(x: Double): Int {
    return ((x + 0.5).toInt())
}

fun digital_differential_analyzer_line(p1: Point, p2: Point): MutableList<Point> {
    var dx: Int = (p2.x - p1.x).toInt()
    var dy: Int = (p2.y - p1.y).toInt()
    var abs_dx: Int = (abs_int(dx)).toInt()
    var abs_dy: Int = (abs_int(dy)).toInt()
    var steps: Int = (if (abs_dx > abs_dy) abs_dx else abs_dy).toInt()
    var x_increment: Double = ((dx.toDouble())) / ((steps.toDouble()))
    var y_increment: Double = ((dy.toDouble())) / ((steps.toDouble()))
    var coordinates: MutableList<Point> = mutableListOf<Point>()
    var x: Double = ((p1.x).toDouble())
    var y: Double = ((p1.y).toDouble())
    var i: Int = (0).toInt()
    while (i < steps) {
        x = x + x_increment
        y = y + y_increment
        var point: Point = Point(x = round_int(x), y = round_int(y))
        coordinates = run { val _tmp = coordinates.toMutableList(); _tmp.add(point); _tmp }
        i = i + 1
    }
    return coordinates
}

fun user_main(): Unit {
    var result: MutableList<Point> = digital_differential_analyzer_line(Point(x = 1, y = 1), Point(x = 4, y = 4))
    println(result)
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
