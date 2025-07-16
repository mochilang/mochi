case class Point(var x: Double, var y: Double)

case class QuadSpline(var c0: Double, var c1: Double, var c2: Double)

case class QuadCurve(var x: QuadSpline, var y: QuadSpline)

object b_zier_curves_intersections {
  def absf(x: Double): Double = {
    if (x < 0) {
      return -x
    }
    return x
  }
  
  def maxf(a: Double, b: Double): Double = {
    if (a > b) {
      return a
    }
    return b
  }
  
  def minf(a: Double, b: Double): Double = {
    if (a < b) {
      return a
    }
    return b
  }
  
  def max3(a: Double, b: Double, c: Double): Double = {
    var m = a
    if (b > m) {
      m = b
    }
    if (c > m) {
      m = c
    }
    return m
  }
  
  def min3(a: Double, b: Double, c: Double): Double = {
    var m = a
    if (b < m) {
      m = b
    }
    if (c < m) {
      m = c
    }
    return m
  }
  
  def subdivideQuadSpline(q: QuadSpline, t: Double): List[QuadSpline] = {
    val s = 1 - t
    var u = QuadSpline(c0 = q.c0, c1 = 0, c2 = 0)
    var v = QuadSpline(c0 = 0, c1 = 0, c2 = q.c2)
    u.c1 = s * q.c0 + t * q.c1
    v.c1 = s * q.c1 + t * q.c2
    u.c2 = s * u.c1 + t * v.c1
    v.c0 = u.c2
    return List(u, v)
  }
  
  def subdivideQuadCurve(q: QuadCurve, t: Double): List[QuadCurve] = {
    val xs = subdivideQuadSpline(q.x, t)
    val ys = subdivideQuadSpline(q.y, t)
    var u = QuadCurve(x = (xs).apply(0), y = (ys).apply(0))
    var v = QuadCurve(x = (xs).apply(1), y = (ys).apply(1))
    return List(u, v)
  }
  
  def rectsOverlap(xa0: Double, ya0: Double, xa1: Double, ya1: Double, xb0: Double, yb0: Double, xb1: Double, yb1: Double): Boolean = xb0 <= xa1 && xa0 <= xb1 && yb0 <= ya1 && ya0 <= yb1
  
  def testIntersect(p: QuadCurve, q: QuadCurve, tol: Double): Map[String, any] = {
    val pxmin = min3(p.x.c0, p.x.c1, p.x.c2)
    val pymin = min3(p.y.c0, p.y.c1, p.y.c2)
    val pxmax = max3(p.x.c0, p.x.c1, p.x.c2)
    val pymax = max3(p.y.c0, p.y.c1, p.y.c2)
    val qxmin = min3(q.x.c0, q.x.c1, q.x.c2)
    val qymin = min3(q.y.c0, q.y.c1, q.y.c2)
    val qxmax = max3(q.x.c0, q.x.c1, q.x.c2)
    val qymax = max3(q.y.c0, q.y.c1, q.y.c2)
    var exclude = true
    var accept = false
    var inter = Point(x = 0, y = 0)
    if (rectsOverlap(pxmin, pymin, pxmax, pymax, qxmin, qymin, qxmax, qymax)) {
      exclude = false
      val xmin = maxf(pxmin, qxmin)
      val xmax = minf(pxmax, qxmax)
      if (xmax - xmin <= tol) {
        val ymin = maxf(pymin, qymin)
        val ymax = minf(pymax, qymax)
        if (ymax - ymin <= tol) {
          accept = true
          inter.x = 0.5 * (xmin + xmax)
          inter.y = 0.5 * (ymin + ymax)
        }
      }
    }
    return Map("exclude" -> exclude, "accept" -> accept, "intersect" -> inter)
  }
  
  def seemsToBeDuplicate(pts: List[Point], xy: Point, spacing: Double): Boolean = {
    var i = 0
    while (i < pts.length) {
      val pt = (pts).apply(i)
      if (absf(pt.x - xy.x) < spacing && absf(pt.y - xy.y) < spacing) {
        return true
      }
      i += 1
    }
    return false
  }
  
  def findIntersects(p: QuadCurve, q: QuadCurve, tol: Double, spacing: Double): List[Point] = {
    var inters: List[Point] = scala.collection.mutable.ArrayBuffer[Any]()
    var workload: List[Map[String, QuadCurve]] = scala.collection.mutable.ArrayBuffer(Map("p" -> p, "q" -> q))
    while (workload.length > 0) {
      val idx = workload.length - 1
      val work = (workload).apply(idx)
      workload = workload.slice(0, idx)
      val res = testIntersect((work).apply("p"), (work).apply("q"), tol)
      val excl = (res).apply("exclude")
      val acc = (res).apply("accept")
      val inter = (res).apply("intersect").asInstanceOf[Point]
      if (acc != null) {
        if (!seemsToBeDuplicate(inters, inter, spacing)) {
          inters = inters :+ inter
        }
      } else {
        if (!(excl != null) != null) {
          val ps = subdivideQuadCurve((work).apply("p"), 0.5)
          val qs = subdivideQuadCurve((work).apply("q"), 0.5)
          val p0 = (ps).apply(0)
          val p1 = (ps).apply(1)
          val q0 = (qs).apply(0)
          val q1 = (qs).apply(1)
          workload = workload :+ Map("p" -> p0, "q" -> q0)
          workload = workload :+ Map("p" -> p0, "q" -> q1)
          workload = workload :+ Map("p" -> p1, "q" -> q0)
          workload = workload :+ Map("p" -> p1, "q" -> q1)
        }
      }
    }
    return inters
  }
  
  def main() = {
    val p = QuadCurve(x = QuadSpline(c0 = -1, c1 = 0, c2 = 1), y = QuadSpline(c0 = 0, c1 = 10, c2 = 0))
    val q = QuadCurve(x = QuadSpline(c0 = 2, c1 = -8, c2 = 2), y = QuadSpline(c0 = 1, c1 = 2, c2 = 3))
    val tol = 1e-07
    val spacing = tol * 10
    val inters = findIntersects(p, q, tol, spacing)
    var i = 0
    while (i < inters.length) {
      val pt = (inters).apply(i)
      println(((("(" + pt.x.toString).asInstanceOf[Int] + ", ").asInstanceOf[Int] + pt.y.toString).asInstanceOf[Int] + ")")
      i += 1
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
