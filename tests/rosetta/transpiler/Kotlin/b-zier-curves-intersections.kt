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

data class Point(var x: Double = 0.0, var y: Double = 0.0)
data class QuadSpline(var c0: Double = 0.0, var c1: Double = 0.0, var c2: Double = 0.0)
data class QuadCurve(var x: QuadSpline = QuadSpline(c0 = 0.0, c1 = 0.0, c2 = 0.0), var y: QuadSpline = QuadSpline(c0 = 0.0, c1 = 0.0, c2 = 0.0))
fun absf(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun maxf(a: Double, b: Double): Double {
    if (a > b) {
        return a
    }
    return b
}

fun minf(a: Double, b: Double): Double {
    if (a < b) {
        return a
    }
    return b
}

fun max3(a: Double, b: Double, c: Double): Double {
    var m: Double = a
    if (b > m) {
        m = b
    }
    if (c > m) {
        m = c
    }
    return m
}

fun min3(a: Double, b: Double, c: Double): Double {
    var m: Double = a
    if (b < m) {
        m = b
    }
    if (c < m) {
        m = c
    }
    return m
}

fun subdivideQuadSpline(q: QuadSpline, t: Double): MutableList<QuadSpline> {
    var s: Double = 1.0 - t
    var u: QuadSpline = QuadSpline(c0 = q.c0, c1 = 0.0, c2 = 0.0)
    var v: QuadSpline = QuadSpline(c0 = 0.0, c1 = 0.0, c2 = q.c2)
    u.c1 = (s * q.c0) + (t * q.c1)
    v.c1 = (s * q.c1) + (t * q.c2)
    u.c2 = (s * u.c1) + (t * v.c1)
    v.c0 = u.c2
    return mutableListOf(u, v)
}

fun subdivideQuadCurve(q: QuadCurve, t: Double): MutableList<QuadCurve> {
    var xs: MutableList<QuadSpline> = subdivideQuadSpline(q.x, t)
    var ys: MutableList<QuadSpline> = subdivideQuadSpline(q.y, t)
    var u: QuadCurve = QuadCurve(x = xs[0]!!, y = ys[0]!!)
    var v: QuadCurve = QuadCurve(x = xs[1]!!, y = ys[1]!!)
    return mutableListOf(u, v)
}

fun rectsOverlap(xa0: Double, ya0: Double, xa1: Double, ya1: Double, xb0: Double, yb0: Double, xb1: Double, yb1: Double): Boolean {
    return ((((((xb0 <= xa1) && (xa0 <= xb1) as Boolean)) && (yb0 <= ya1) as Boolean)) && (ya0 <= yb1)) as Boolean
}

fun testIntersect(p: QuadCurve, q: QuadCurve, tol: Double): MutableMap<String, Any?> {
    var pxmin: Double = min3(p.x.c0, p.x.c1, p.x.c2)
    var pymin: Double = min3(p.y.c0, p.y.c1, p.y.c2)
    var pxmax: Double = max3(p.x.c0, p.x.c1, p.x.c2)
    var pymax: Double = max3(p.y.c0, p.y.c1, p.y.c2)
    var qxmin: Double = min3(q.x.c0, q.x.c1, q.x.c2)
    var qymin: Double = min3(q.y.c0, q.y.c1, q.y.c2)
    var qxmax: Double = max3(q.x.c0, q.x.c1, q.x.c2)
    var qymax: Double = max3(q.y.c0, q.y.c1, q.y.c2)
    var exclude: Boolean = true
    var accept: Boolean = false
    var inter: Point = Point(x = 0.0, y = 0.0)
    if ((rectsOverlap(pxmin, pymin, pxmax, pymax, qxmin, qymin, qxmax, qymax)) as Boolean) {
        exclude = false
        var xmin: Double = maxf(pxmin, qxmin)
        var xmax: Double = minf(pxmax, qxmax)
        if ((xmax - xmin) <= tol) {
            var ymin: Double = maxf(pymin, qymin)
            var ymax: Double = minf(pymax, qymax)
            if ((ymax - ymin) <= tol) {
                accept = true
                inter.x = 0.5 * (xmin + xmax)
                inter.y = 0.5 * (ymin + ymax)
            }
        }
    }
    return mutableMapOf<String, Any?>("exclude" to (exclude), "accept" to (accept), "intersect" to (inter))
}

fun seemsToBeDuplicate(pts: MutableList<Point>, xy: Point, spacing: Double): Boolean {
    var i: Int = 0
    while (i < pts.size) {
        var pt: Point = pts[i]!!
        if ((absf(pt.x - xy.x) < spacing) && (absf(pt.y - xy.y) < spacing)) {
            return true
        }
        i = i + 1
    }
    return false
}

fun findIntersects(p: QuadCurve, q: QuadCurve, tol: Double, spacing: Double): MutableList<Point> {
    var inters: MutableList<Point> = mutableListOf<Point>()
    var workload: MutableList<MutableMap<String, QuadCurve>> = mutableListOf(mutableMapOf<String, QuadCurve>("p" to (p), "q" to (q)))
    while (workload.size > 0) {
        var idx: BigInteger = (workload.size - 1).toBigInteger()
        var work: MutableMap<String, QuadCurve> = workload[(idx).toInt()]!!
        workload = workload.subList(0, (idx).toInt())
        var res: MutableMap<String, Any?> = testIntersect((work)["p"] as QuadCurve, (work)["q"] as QuadCurve, tol)
        var excl: Any? = (res)["exclude"] as Any?
        var acc: Any? = (res)["accept"] as Any?
        var inter: Point = ((res)["intersect"] as Any?) as Point
        if (acc as Boolean) {
            if (!seemsToBeDuplicate(inters, inter, spacing)) {
                inters = run { val _tmp = inters.toMutableList(); _tmp.add(inter); _tmp } as MutableList<Point>
            }
        } else {
            if (!(excl as Boolean)) {
                var ps: MutableList<QuadCurve> = subdivideQuadCurve((work)["p"] as QuadCurve, 0.5)
                var qs: MutableList<QuadCurve> = subdivideQuadCurve((work)["q"] as QuadCurve, 0.5)
                var p0: QuadCurve = ps[0]!!
                var p1: QuadCurve = ps[1]!!
                var q0: QuadCurve = qs[0]!!
                var q1: QuadCurve = qs[1]!!
                workload = run { val _tmp = workload.toMutableList(); _tmp.add(mutableMapOf<String, QuadCurve>("p" to (p0), "q" to (q0))); _tmp } as MutableList<MutableMap<String, QuadCurve>>
                workload = run { val _tmp = workload.toMutableList(); _tmp.add(mutableMapOf<String, QuadCurve>("p" to (p0), "q" to (q1))); _tmp } as MutableList<MutableMap<String, QuadCurve>>
                workload = run { val _tmp = workload.toMutableList(); _tmp.add(mutableMapOf<String, QuadCurve>("p" to (p1), "q" to (q0))); _tmp } as MutableList<MutableMap<String, QuadCurve>>
                workload = run { val _tmp = workload.toMutableList(); _tmp.add(mutableMapOf<String, QuadCurve>("p" to (p1), "q" to (q1))); _tmp } as MutableList<MutableMap<String, QuadCurve>>
            }
        }
    }
    return inters
}

fun user_main(): Unit {
    var p: QuadCurve = QuadCurve(x = QuadSpline(c0 = 0.0 - 1.0, c1 = 0.0, c2 = 1.0), y = QuadSpline(c0 = 0.0, c1 = 10.0, c2 = 0.0))
    var q: QuadCurve = QuadCurve(x = QuadSpline(c0 = 2.0, c1 = 0.0 - 8.0, c2 = 2.0), y = QuadSpline(c0 = 1.0, c1 = 2.0, c2 = 3.0))
    var tol: Double = 0.0000001
    var spacing: Double = tol * 10.0
    var inters: MutableList<Point> = findIntersects(p, q, tol, spacing)
    var i: Int = 0
    while (i < inters.size) {
        var pt: Point = inters[i]!!
        println(((("(" + pt.x.toString()) + ", ") + pt.y.toString()) + ")")
        i = i + 1
    }
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
