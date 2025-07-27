import java.math.BigInteger

data class Point(var x: Double, var y: Double)
data class QuadSpline(var c0: Double, var c1: Double, var c2: Double)
data class QuadCurve(var x: QuadSpline, var y: QuadSpline)
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
    val s: Double = 1.0 - t
    var u: QuadSpline = QuadSpline(c0 = q.c0, c1 = 0.0, c2 = 0.0)
    var v: QuadSpline = QuadSpline(c0 = 0.0, c1 = 0.0, c2 = q.c2)
    u.c1 = (s * q.c0) + (t * q.c1)
    v.c1 = (s * q.c1) + (t * q.c2)
    u.c2 = (s * u.c1) + (t * v.c1)
    v.c0 = u.c2
    return mutableListOf(u, v)
}

fun subdivideQuadCurve(q: QuadCurve, t: Double): MutableList<QuadCurve> {
    val xs: MutableList<QuadSpline> = subdivideQuadSpline(q.x, t)
    val ys: MutableList<QuadSpline> = subdivideQuadSpline(q.y, t)
    var u: QuadCurve = QuadCurve(x = xs[0], y = ys[0])
    var v: QuadCurve = QuadCurve(x = xs[1], y = ys[1])
    return mutableListOf(u, v)
}

fun rectsOverlap(xa0: Double, ya0: Double, xa1: Double, ya1: Double, xb0: Double, yb0: Double, xb1: Double, yb1: Double): Boolean {
    return ((((((xb0 <= xa1) && (xa0 <= xb1) as Boolean)) && (yb0 <= ya1) as Boolean)) && (ya0 <= yb1)) as Boolean
}

fun testIntersect(p: QuadCurve, q: QuadCurve, tol: Double): MutableMap<String, Any?> {
    val pxmin: Double = min3(p.x.c0, p.x.c1, p.x.c2)
    val pymin: Double = min3(p.y.c0, p.y.c1, p.y.c2)
    val pxmax: Double = max3(p.x.c0, p.x.c1, p.x.c2)
    val pymax: Double = max3(p.y.c0, p.y.c1, p.y.c2)
    val qxmin: Double = min3(q.x.c0, q.x.c1, q.x.c2)
    val qymin: Double = min3(q.y.c0, q.y.c1, q.y.c2)
    val qxmax: Double = max3(q.x.c0, q.x.c1, q.x.c2)
    val qymax: Double = max3(q.y.c0, q.y.c1, q.y.c2)
    var exclude: Boolean = true
    var accept: Boolean = false
    var inter: Point = Point(x = 0.0, y = 0.0)
    if ((rectsOverlap(pxmin, pymin, pxmax, pymax, qxmin, qymin, qxmax, qymax)) as Boolean) {
        exclude = false
        val xmin: Double = maxf(pxmin, qxmin)
        val xmax: Double = minf(pxmax, qxmax)
        if ((xmax - xmin) <= tol) {
            val ymin: Double = maxf(pymin, qymin)
            val ymax: Double = minf(pymax, qymax)
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
        val pt: Point = pts[i]
        if ((absf(pt.x - xy.x) < spacing) && (absf(pt.y - xy.y) < spacing)) {
            return true
        }
        i = i + 1
    }
    return false
}

fun findIntersects(p: QuadCurve, q: QuadCurve, tol: Double, spacing: Double): MutableList<Point> {
    var inters: MutableList<Point> = mutableListOf()
    var workload: MutableList<MutableMap<String, QuadCurve>> = mutableListOf(mutableMapOf<String, QuadCurve>("p" to (p), "q" to (q)))
    while (workload.size > 0) {
        val idx: Int = workload.size - 1
        val work: MutableMap<String, QuadCurve> = workload[idx]
        workload = workload.subList(0, idx)
        val res: MutableMap<String, Any?> = testIntersect((work)["p"] as QuadCurve, (work)["q"] as QuadCurve, tol)
        val excl: Any? = (res)["exclude"] as Any?
        val acc: Any? = (res)["accept"] as Any?
        val inter: Point = ((res)["intersect"] as Any?) as Point
        if (acc as Boolean) {
            if (!seemsToBeDuplicate(inters, inter, spacing)) {
                inters = run { val _tmp = inters.toMutableList(); _tmp.add(inter); _tmp } as MutableList<Point>
            }
        } else {
            if (!(excl as Boolean)) {
                val ps: MutableList<QuadCurve> = subdivideQuadCurve((work)["p"] as QuadCurve, 0.5)
                val qs: MutableList<QuadCurve> = subdivideQuadCurve((work)["q"] as QuadCurve, 0.5)
                val p0: QuadCurve = ps[0]
                val p1: QuadCurve = ps[1]
                val q0: QuadCurve = qs[0]
                val q1: QuadCurve = qs[1]
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
    val p: QuadCurve = QuadCurve(x = QuadSpline(c0 = 0.0 - 1.0, c1 = 0.0, c2 = 1.0), y = QuadSpline(c0 = 0.0, c1 = 10.0, c2 = 0.0))
    val q: QuadCurve = QuadCurve(x = QuadSpline(c0 = 2.0, c1 = 0.0 - 8.0, c2 = 2.0), y = QuadSpline(c0 = 1.0, c1 = 2.0, c2 = 3.0))
    val tol: Double = 0.0000001
    val spacing: Double = tol * 10.0
    val inters: MutableList<Point> = findIntersects(p, q, tol, spacing)
    var i: Int = 0
    while (i < inters.size) {
        val pt: Point = inters[i]
        println(((("(" + pt.x.toString()) + ", ") + pt.y.toString()) + ")")
        i = i + 1
    }
}

fun main() {
    user_main()
}
