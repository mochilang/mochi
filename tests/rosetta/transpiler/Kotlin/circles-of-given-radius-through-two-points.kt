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

data class Point(var x: Double = 0.0, var y: Double = 0.0)
var Two: String = "Two circles."
var R0: String = "R==0.0 does not describe circles."
var Co: String = "Coincident points describe an infinite number of circles."
var CoR0: String = "Coincident points with r==0.0 describe a degenerate circle."
var Diam: String = "Points form a diameter and describe only a single circle."
var Far: String = "Points too far apart to form circles."
var td: MutableList<MutableList<Any?>> = mutableListOf(mutableListOf<Any?>((Point(x = 0.1234, y = 0.9876) as Any?), (Point(x = 0.8765, y = 0.2345) as Any?), (2.0 as Any?)), mutableListOf<Any?>((Point(x = 0.0, y = 2.0) as Any?), (Point(x = 0.0, y = 0.0) as Any?), (1.0 as Any?)), mutableListOf<Any?>((Point(x = 0.1234, y = 0.9876) as Any?), (Point(x = 0.1234, y = 0.9876) as Any?), (2.0 as Any?)), mutableListOf<Any?>((Point(x = 0.1234, y = 0.9876) as Any?), (Point(x = 0.8765, y = 0.2345) as Any?), (0.5 as Any?)), mutableListOf<Any?>((Point(x = 0.1234, y = 0.9876) as Any?), (Point(x = 0.1234, y = 0.9876) as Any?), (0.0 as Any?)))
fun sqrtApprox(x: Double): Double {
    var g: Double = x
    var i: Int = 0
    while (i < 40) {
        g = (g + (x / g)) / 2.0
        i = i + 1
    }
    return g
}

fun hypot(x: Double, y: Double): Double {
    return sqrtApprox((x * x) + (y * y))
}

fun circles(p1: Point, p2: Point, r: Double): MutableList<Any?> {
    if ((p1.x == p2.x) && (p1.y == p2.y)) {
        if (r == 0.0) {
            return mutableListOf<Any?>((p1 as Any?), (p1 as Any?), ("Coincident points with r==0.0 describe a degenerate circle." as Any?))
        }
        return mutableListOf<Any?>((p1 as Any?), (p2 as Any?), ("Coincident points describe an infinite number of circles." as Any?))
    }
    if (r == 0.0) {
        return mutableListOf<Any?>((p1 as Any?), (p2 as Any?), ("R==0.0 does not describe circles." as Any?))
    }
    var dx: Double = p2.x - p1.x
    var dy: Double = p2.y - p1.y
    var q: Double = hypot(dx, dy)
    if (q > (2.0 * r)) {
        return mutableListOf<Any?>((p1 as Any?), (p2 as Any?), ("Points too far apart to form circles." as Any?))
    }
    var m: Point = Point(x = (p1.x + p2.x) / 2.0, y = (p1.y + p2.y) / 2.0)
    if (q == (2.0 * r)) {
        return mutableListOf<Any?>((m as Any?), (m as Any?), ("Points form a diameter and describe only a single circle." as Any?))
    }
    var d: Double = sqrtApprox((r * r) - ((q * q) / 4.0))
    var ox: Double = (d * dx) / q
    var oy: Double = (d * dy) / q
    return mutableListOf<Any?>((Point(x = m.x - oy, y = m.y + ox) as Any?), (Point(x = m.x + oy, y = m.y - ox) as Any?), ("Two circles." as Any?))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (tc in td) {
            var p1: Any? = tc[0] as Any?
            var p2: Any? = tc[1] as Any?
            var r: Any? = tc[2] as Any?
            println(((("p1:  {" + ((p1 as Point)).x.toString()) + " ") + ((p1 as Point)).y.toString()) + "}")
            println(((("p2:  {" + ((p2 as Point)).x.toString()) + " ") + ((p2 as Point)).y.toString()) + "}")
            println("r:  " + r.toString())
            var res: MutableList<Any?> = circles((p1 as Point), (p2 as Point), (r as Double))
            var c1: Any? = res[0] as Any?
            var c2: Any? = res[1] as Any?
            var caseStr: Any? = res[2] as Any?
            println("   " + (caseStr).toString())
            if ((caseStr == "Points form a diameter and describe only a single circle.") || (caseStr == "Coincident points with r==0.0 describe a degenerate circle.")) {
                println(((("   Center:  {" + ((c1 as Point)).x.toString()) + " ") + ((c1 as Point)).y.toString()) + "}")
            } else {
                if (caseStr == "Two circles.") {
                    println(((("   Center 1:  {" + ((c1 as Point)).x.toString()) + " ") + ((c1 as Point)).y.toString()) + "}")
                    println(((("   Center 2:  {" + ((c2 as Point)).x.toString()) + " ") + ((c2 as Point)).y.toString()) + "}")
                }
            }
            println("")
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
