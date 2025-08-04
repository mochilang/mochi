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
var pts: MutableList<Point> = mutableListOf(Point(x = 16, y = 3), Point(x = 12, y = 17), Point(x = 0, y = 6), Point(x = 0 - 4, y = 0 - 6), Point(x = 16, y = 6), Point(x = 16, y = 0 - 7), Point(x = 16, y = 0 - 3), Point(x = 17, y = 0 - 4), Point(x = 5, y = 19), Point(x = 19, y = 0 - 8), Point(x = 3, y = 16), Point(x = 12, y = 13), Point(x = 3, y = 0 - 4), Point(x = 17, y = 5), Point(x = 0 - 3, y = 15), Point(x = 0 - 3, y = 0 - 9), Point(x = 0, y = 11), Point(x = 0 - 9, y = 0 - 3), Point(x = 0 - 4, y = 0 - 2), Point(x = 12, y = 10))
var hull: MutableList<Point> = convexHull(pts)
fun ccw(a: Point, b: Point, c: Point): Boolean {
    var lhs: BigInteger = ((b.x - a.x) * (c.y - a.y)).toBigInteger()
    var rhs: BigInteger = ((b.y - a.y) * (c.x - a.x)).toBigInteger()
    return lhs.compareTo((rhs)) > 0
}

fun sortPoints(ps: MutableList<Point>): MutableList<Point> {
    var arr: MutableList<Point> = ps
    var n: Int = arr.size
    var i: Int = 0
    while (i < n) {
        var j: Int = 0
        while (j < (n - 1)) {
            var p: Point = arr[j]!!
            var q: Point = arr[j + 1]!!
            if ((p.x > q.x) || (((p.x == q.x) && (p.y > q.y) as Boolean))) {
                arr[j] = q
                arr[j + 1] = p
            }
            j = j + 1
        }
        i = i + 1
    }
    return arr
}

fun convexHull(ps: MutableList<Point>): MutableList<Point> {
    var ps: MutableList<Point> = ps
    ps = sortPoints(ps)
    var h: MutableList<Point> = mutableListOf<Point>()
    for (pt in ps) {
        while ((h.size >= 2) && (ccw(h[h.size - 2]!!, h[h.size - 1]!!, pt) == false)) {
            h = h.subList(0, h.size - 1)
        }
        h = run { val _tmp = h.toMutableList(); _tmp.add(pt); _tmp }
    }
    var i: BigInteger = (ps.size - 2).toBigInteger()
    var t: BigInteger = (h.size + 1).toBigInteger()
    while (i.compareTo((0).toBigInteger()) >= 0) {
        var pt: Point = ps[(i).toInt()]!!
        while (((h.size).toBigInteger().compareTo((t)) >= 0) && (ccw(h[h.size - 2]!!, h[h.size - 1]!!, pt) == false)) {
            h = h.subList(0, h.size - 1)
        }
        h = run { val _tmp = h.toMutableList(); _tmp.add(pt); _tmp }
        i = i.subtract((1).toBigInteger())
    }
    return h.subList(0, h.size - 1)
}

fun pointStr(p: Point): String {
    return ((("(" + p.x.toString()) + ",") + p.y.toString()) + ")"
}

fun hullStr(h: MutableList<Point>): String {
    var s: String = "["
    var i: Int = 0
    while (i < h.size) {
        s = s + pointStr(h[i]!!)
        if (i < (h.size - 1)) {
            s = s + " "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Convex Hull: " + hullStr(hull))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
