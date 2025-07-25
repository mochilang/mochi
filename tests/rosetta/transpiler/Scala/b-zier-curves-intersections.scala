// Generated by Mochi v0.10.40 on 2025-07-26 00:02:51 GMT+7
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.collection.immutable.ListMap
object Main {
  private var _nowSeed: Long = 0L
  private var _nowSeeded: Boolean = false
  private def _now(): Int = {
    if (!_nowSeeded) {
      sys.env.get("MOCHI_NOW_SEED").foreach { s =>
      try { _nowSeed = s.toInt; _nowSeeded = true } catch { case _ : NumberFormatException => () }
    }
  }
  if (_nowSeeded) {
    _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
    _nowSeed.toInt
  } else {
    Math.abs((System.nanoTime() / 1000).toInt)
  }
}

def toJson(value: Any, indent: Int = 0): String = value match {
  case m: scala.collection.Map[_, _] =>
  val items = ListMap(m.toSeq.sortBy(_._1.toString): _*).toSeq.map{ case (k,v) => "  "*(indent+1)+"\""+k.toString+"\": "+toJson(v, indent+1) }
  "{\n"+items.mkString(",\n")+"\n"+"  "*indent+"}"
  case s: Seq[_] =>
  val items = s.map(x => "  "*(indent+1)+toJson(x, indent+1))
  "[\n"+items.mkString(",\n")+"\n"+"  "*indent+"]"
  case s: String => "\""+s+"\""
  case other => other.toString
}

case class Point(var x: Double, var y: Double)

case class QuadSpline(var c0: Double, var c1: Double, var c2: Double)

case class QuadCurve(var x: QuadSpline, var y: QuadSpline)

def main(args: Array[String]): Unit = {
  {
    System.gc()
    val _startMem = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val _start = _now()
    def absf(x: Double): Double = {
      if (x < 0.0) {
        return (0 - x).toString.toDouble
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
      var m: Double = a
      if (b > m) {
        m = b
      }
      if (c > m) {
        m = c
      }
      return m
    }
    def min3(a: Double, b: Double, c: Double): Double = {
      var m: Double = a
      if (b < m) {
        m = b
      }
      if (c < m) {
        m = c
      }
      return m
    }
    def subdivideQuadSpline(q: QuadSpline, t: Double): ArrayBuffer[QuadSpline] = {
      val s: Double = 1.0 - t
      var u: QuadSpline = QuadSpline(q.c0, 0.0, 0.0)
      var v: QuadSpline = QuadSpline(0.0, 0.0, q.c2)
      u.c1 = (s * q.c0).toString.toDouble + (t * q.c1).toString.toDouble
      v.c1 = (s * q.c1).toString.toDouble + (t * q.c2).toString.toDouble
      u.c2 = (s * u.c1).toString.toDouble + (t * v.c1).toString.toDouble
      v.c0 = u.c2
      return (ArrayBuffer(u, v)).asInstanceOf[ArrayBuffer[QuadSpline]]
    }
    def subdivideQuadCurve(q: QuadCurve, t: Double): ArrayBuffer[QuadCurve] = {
      val xs: ArrayBuffer[QuadSpline] = subdivideQuadSpline(q.x, t)
      val ys: ArrayBuffer[QuadSpline] = subdivideQuadSpline(q.y, t)
      var u: QuadCurve = QuadCurve(xs(0), ys(0))
      var v: QuadCurve = QuadCurve(xs(1), ys(1))
      return (ArrayBuffer(u, v)).asInstanceOf[ArrayBuffer[QuadCurve]]
    }
    def rectsOverlap(xa0: Double, ya0: Double, xa1: Double, ya1: Double, xb0: Double, yb0: Double, xb1: Double, yb1: Double): Boolean = {
      return (((xb0 <= xa1 && xa0 <= xb1).asInstanceOf[Boolean] && yb0 <= ya1).asInstanceOf[Boolean] && ya0 <= yb1).asInstanceOf[Boolean]
    }
    def testIntersect(p: QuadCurve, q: QuadCurve, tol: Double): Map[String,Any] = {
      val pxmin: Double = min3((p.x.c0).toString.toDouble, (p.x.c1).toString.toDouble, (p.x.c2).toString.toDouble)
      val pymin: Double = min3((p.y.c0).toString.toDouble, (p.y.c1).toString.toDouble, (p.y.c2).toString.toDouble)
      val pxmax: Double = max3((p.x.c0).toString.toDouble, (p.x.c1).toString.toDouble, (p.x.c2).toString.toDouble)
      val pymax: Double = max3((p.y.c0).toString.toDouble, (p.y.c1).toString.toDouble, (p.y.c2).toString.toDouble)
      val qxmin: Double = min3((q.x.c0).toString.toDouble, (q.x.c1).toString.toDouble, (q.x.c2).toString.toDouble)
      val qymin: Double = min3((q.y.c0).toString.toDouble, (q.y.c1).toString.toDouble, (q.y.c2).toString.toDouble)
      val qxmax: Double = max3((q.x.c0).toString.toDouble, (q.x.c1).toString.toDouble, (q.x.c2).toString.toDouble)
      val qymax: Double = max3((q.y.c0).toString.toDouble, (q.y.c1).toString.toDouble, (q.y.c2).toString.toDouble)
      var exclude: Boolean = true
      var accept: Boolean = false
      var inter: Point = Point(0.0, 0.0)
      if (rectsOverlap(pxmin, pymin, pxmax, pymax, qxmin, qymin, qxmax, qymax)) {
        exclude = false
        val xmin: Double = maxf(pxmin, qxmin)
        val xmax: Double = minf(pxmax, qxmax)
        if (xmax - xmin <= tol) {
          val ymin: Double = maxf(pymin, qymin)
          val ymax: Double = minf(pymax, qymax)
          if (ymax - ymin <= tol) {
            accept = true
            inter.x = 0.5 * (xmin + xmax)
            inter.y = 0.5 * (ymin + ymax)
          }
        }
      }
      return Map("exclude" -> (exclude), "accept" -> (accept), "intersect" -> (inter))
    }
    def seemsToBeDuplicate(pts: ArrayBuffer[Point], xy: Point, spacing: Double): Boolean = {
      var i: Int = 0
      while (i < (pts).size) {
        val pt: Point = pts(i)
        if ((absf((pt.x - xy.x).toString.toDouble) < spacing && absf((pt.y - xy.y).toString.toDouble) < spacing).asInstanceOf[Boolean]) {
          return true
        }
        i = (i + 1).asInstanceOf[Int]
      }
      return false
    }
    def findIntersects(p: QuadCurve, q: QuadCurve, tol: Double, spacing: Double): ArrayBuffer[Point] = {
      var inters: ArrayBuffer[Point] = ArrayBuffer()
      var workload: ArrayBuffer[Map[String,QuadCurve]] = ArrayBuffer(Map("p" -> (p), "q" -> (q)))
      while ((workload).size > 0) {
        val idx: Int = (workload).size - 1
        val work: Map[String,QuadCurve] = workload(idx)
        workload = workload.slice(0, idx)
        val res: Map[String,Any] = testIntersect(work.getOrElse("p", null.asInstanceOf[QuadCurve]), work.getOrElse("q", null.asInstanceOf[QuadCurve]), tol)
        val excl = res.getOrElse("exclude", null.asInstanceOf[Any])
        val acc = res.getOrElse("accept", null.asInstanceOf[Any])
        val inter: Point = (res.getOrElse("intersect", null.asInstanceOf[Any])).asInstanceOf[Point]
        if (acc.asInstanceOf[Boolean]) {
          if ((!seemsToBeDuplicate(inters, inter, spacing)).asInstanceOf[Boolean]) {
            inters = inters :+ inter
          }
        } else {
          if ((!excl.asInstanceOf[Boolean]).asInstanceOf[Boolean]) {
            val ps: ArrayBuffer[QuadCurve] = subdivideQuadCurve(work.getOrElse("p", null.asInstanceOf[QuadCurve]), 0.5)
            val qs: ArrayBuffer[QuadCurve] = subdivideQuadCurve(work.getOrElse("q", null.asInstanceOf[QuadCurve]), 0.5)
            val p0: QuadCurve = ps(0)
            val p1: QuadCurve = ps(1)
            val q0: QuadCurve = qs(0)
            val q1: QuadCurve = qs(1)
            workload = workload :+ Map("p" -> (p0), "q" -> (q0))
            workload = workload :+ Map("p" -> (p0), "q" -> (q1))
            workload = workload :+ Map("p" -> (p1), "q" -> (q0))
            workload = workload :+ Map("p" -> (p1), "q" -> (q1))
          }
        }
      }
      return inters
    }
    def main(): Any = {
      val p: QuadCurve = QuadCurve(QuadSpline(0 - 1.0, 0.0, 1.0), QuadSpline(0.0, 10.0, 0.0))
      val q: QuadCurve = QuadCurve(QuadSpline(2.0, 0 - 8.0, 2.0), QuadSpline(1.0, 2.0, 3.0))
      val tol: Double = 0.0000001
      val spacing: Double = tol * 10.0
      val inters: ArrayBuffer[Point] = findIntersects(p, q, tol, spacing)
      var i: Int = 0
      while (i < (inters).size) {
        val pt: Point = inters(i)
        println("(" + String.valueOf(pt.x) + ", " + String.valueOf(pt.y) + ")")
        i = (i + 1).asInstanceOf[Int]
      }
    }
    main()
    val _end = _now()
    System.gc()
    val _endMem = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val _durUs = (_end - _start) / 1000
    var _memDiff = _endMem - _startMem
    if (_memDiff <= 0) _memDiff = _endMem
    println(toJson(scala.collection.immutable.Map("duration_us" -> _durUs, "memory_bytes" -> _memDiff, "name" -> "main")))
  }
}
}
