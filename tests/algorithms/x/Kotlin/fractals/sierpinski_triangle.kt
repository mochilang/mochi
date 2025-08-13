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
fun get_mid(p1: Point, p2: Point): Point {
    return Point(x = (p1.x + p2.x) / 2, y = (p1.y + p2.y) / 2)
}

fun point_to_string(p: Point): String {
    return ((("(" + p.x.toString()) + ",") + p.y.toString()) + ")"
}

fun triangle(v1: Point, v2: Point, v3: Point, depth: Int): Unit {
    println((((point_to_string(v1) + " ") + point_to_string(v2)) + " ") + point_to_string(v3))
    if (depth == 0) {
        return
    }
    triangle(v1, get_mid(v1, v2), get_mid(v1, v3), depth - 1)
    triangle(v2, get_mid(v1, v2), get_mid(v2, v3), depth - 1)
    triangle(v3, get_mid(v3, v2), get_mid(v1, v3), depth - 1)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        triangle(Point(x = 0 - 175, y = 0 - 125), Point(x = 0, y = 175), Point(x = 175, y = 0 - 125), 2)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
