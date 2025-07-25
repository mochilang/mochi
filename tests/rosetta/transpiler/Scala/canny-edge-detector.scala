// Generated by Mochi v0.10.40 on 2025-07-26 09:47:34 GMT+7
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.math.BigInt
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

def main(args: Array[String]): Unit = {
  {
    System.gc()
    val _startMem = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val _start = _now()
    val PI: Double = 3.141592653589793
    def conv2d(img: ArrayBuffer[ArrayBuffer[Double]], k: ArrayBuffer[ArrayBuffer[Double]]): ArrayBuffer[ArrayBuffer[Double]] = {
      val h: Int = (img).size
      val w: Int = (img((BigInt(0)).toInt)).size
      val n: Int = (k).size
      val half = n / BigInt(2)
      var out: ArrayBuffer[ArrayBuffer[Double]] = ArrayBuffer()
      var y: BigInt = BigInt(0)
      while (y < h) {
        var row: ArrayBuffer[Double] = ArrayBuffer()
        var x: BigInt = BigInt(0)
        while (x < w) {
          var sum: Double = 0.0
          var j: BigInt = BigInt(0)
          while (j < n) {
            var i: BigInt = BigInt(0)
            while (i < n) {
              var yy = y + j - half
              if (yy < BigInt(0)) {
                yy = BigInt(0)
              }
              if (yy >= h) {
                yy = (h - BigInt(1)).asInstanceOf[BigInt]
              }
              var xx = x + i - half
              if (xx < BigInt(0)) {
                xx = BigInt(0)
              }
              if (xx >= w) {
                xx = (w - BigInt(1)).asInstanceOf[BigInt]
              }
              sum = (sum + img((yy).toInt)((xx).toInt) * k((j).toInt)((i).toInt)).toString.toDouble
              i = (i + BigInt(1)).asInstanceOf[BigInt]
            }
            j = (j + BigInt(1)).asInstanceOf[BigInt]
          }
          row = row :+ sum
          x = (x + BigInt(1)).asInstanceOf[BigInt]
        }
        out = out :+ row
        y = (y + BigInt(1)).asInstanceOf[BigInt]
      }
      return out
    }
    def gradient(img: ArrayBuffer[ArrayBuffer[Double]]): ArrayBuffer[ArrayBuffer[Double]] = {
      val hx: ArrayBuffer[ArrayBuffer[Double]] = ArrayBuffer(ArrayBuffer(BigInt(0) - 1.0, 0.0, 1.0), ArrayBuffer(BigInt(0) - 2.0, 0.0, 2.0), ArrayBuffer(BigInt(0) - 1.0, 0.0, 1.0))
      val hy: ArrayBuffer[ArrayBuffer[Double]] = ArrayBuffer(ArrayBuffer(1.0, 2.0, 1.0), ArrayBuffer(0.0, 0.0, 0.0), ArrayBuffer(BigInt(0) - 1.0, BigInt(0) - 2.0, BigInt(0) - 1.0))
      var gx: ArrayBuffer[ArrayBuffer[Double]] = conv2d(img, hx)
      var gy: ArrayBuffer[ArrayBuffer[Double]] = conv2d(img, hy)
      var h: Int = (img).size
      var w: Int = (img((BigInt(0)).toInt)).size
      var out: ArrayBuffer[ArrayBuffer[Double]] = ArrayBuffer()
      var y: BigInt = BigInt(0)
      while (y < h) {
        var row: ArrayBuffer[Double] = ArrayBuffer()
        var x: BigInt = BigInt(0)
        while (x < w) {
          val g: Int = gx((y).toInt)((x).toInt) * gx((y).toInt)((x).toInt) + gy((y).toInt)((x).toInt) * gy((y).toInt)((x).toInt)
          row = row :+ g.toString.toDouble
          x = (x + BigInt(1)).asInstanceOf[BigInt]
        }
        out = out :+ row
        y = (y + BigInt(1)).asInstanceOf[BigInt]
      }
      return out
    }
    def threshold(g: ArrayBuffer[ArrayBuffer[Double]], t: Double): ArrayBuffer[ArrayBuffer[BigInt]] = {
      var h: Int = (g).size
      var w: Int = (g((BigInt(0)).toInt)).size
      var out: ArrayBuffer[ArrayBuffer[BigInt]] = ArrayBuffer()
      var y: BigInt = BigInt(0)
      while (y < h) {
        var row: ArrayBuffer[BigInt] = ArrayBuffer()
        var x: BigInt = BigInt(0)
        while (x < w) {
          if (g((y).toInt)((x).toInt) >= t) {
            row = row :+ BigInt(1)
          } else {
            row = row :+ BigInt(0)
          }
          x = (x + BigInt(1)).asInstanceOf[BigInt]
        }
        out = out :+ row
        y = (y + BigInt(1)).asInstanceOf[BigInt]
      }
      return out
    }
    def printMatrix(m: ArrayBuffer[ArrayBuffer[BigInt]]): Any = {
      var y: BigInt = BigInt(0)
      while (y < (m).size) {
        var line: String = ""
        var x: BigInt = BigInt(0)
        while (x < (m((BigInt(0)).toInt)).size) {
          line = (line + String.valueOf(m((y).toInt)((x).toInt))).toString
          if (x < (m((BigInt(0)).toInt)).size - BigInt(1)) {
            line = line + " "
          }
          x = (x + BigInt(1)).asInstanceOf[BigInt]
        }
        println(line)
        y = (y + BigInt(1)).asInstanceOf[BigInt]
      }
    }
    def main(): Any = {
      val img: ArrayBuffer[ArrayBuffer[Double]] = ArrayBuffer(ArrayBuffer(0.0, 0.0, 0.0, 0.0, 0.0), ArrayBuffer(0.0, 255.0, 255.0, 255.0, 0.0), ArrayBuffer(0.0, 255.0, 255.0, 255.0, 0.0), ArrayBuffer(0.0, 255.0, 255.0, 255.0, 0.0), ArrayBuffer(0.0, 0.0, 0.0, 0.0, 0.0))
      val g: ArrayBuffer[ArrayBuffer[Double]] = gradient(img)
      val edges: ArrayBuffer[ArrayBuffer[BigInt]] = threshold(g, 1020.0 * 1020.0)
      printMatrix(edges)
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
