// Generated by Mochi v0.10.40 on 2025-07-26 00:02:45 GMT+7
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

def main(args: Array[String]): Unit = {
  {
    System.gc()
    val _startMem = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val _start = _now()
    def indexOf(s: String, ch: String): Int = {
      var i: Int = 0
      while (i < (s).size) {
        if (s.substring(i, i + 1) == ch) {
          return i
        }
        i = (i + 1).asInstanceOf[Int]
      }
      return 0 - 1
    }
    def fmt3(x: Double): String = {
      var y: Double = ((x * 1000.0 + 0.5).asInstanceOf[Int]).toString.toDouble / 1000.0
      var s = String.valueOf(y)
      var dot: Int = indexOf(s, ".")
      if (dot == 0 - 1) {
        s = s + ".000"
      } else {
        var decs = (s).size - dot - 1
        if (decs > 3) {
          s = s.substring(0, dot + 4)
        } else {
          while (decs < 3) {
            s = s + "0"
            decs = (decs + 1).asInstanceOf[Int]
          }
        }
      }
      return s
    }
    def pad(s: String, width: Int): String = {
      var out: String = s
      while ((out).size < width) {
        out = " " + out
      }
      return out
    }
    def smaSeries(xs: ArrayBuffer[Double], period: Int): ArrayBuffer[Double] = {
      var res: ArrayBuffer[Double] = ArrayBuffer()
      var sum: Double = 0.0
      var i: Int = 0
      while (i < (xs).size) {
        sum = (sum + xs(i)).toString.toDouble
        if (i >= period) {
          sum = (sum - xs(i - period)).toString.toDouble
        }
        var denom = i + 1
        if (denom > period) {
          denom = period
        }
        res = res :+ sum / denom.toString.toDouble
        i = (i + 1).asInstanceOf[Int]
      }
      return res
    }
    def main(): Any = {
      var xs: ArrayBuffer[Double] = ArrayBuffer(1.0, 2.0, 3.0, 4.0, 5.0, 5.0, 4.0, 3.0, 2.0, 1.0)
      var sma3: ArrayBuffer[Double] = smaSeries(xs, 3)
      var sma5: ArrayBuffer[Double] = smaSeries(xs, 5)
      println("x       sma3   sma5")
      var i: Int = 0
      while (i < (xs).size) {
        val line: String = pad(fmt3(xs(i)), 5) + "  " + pad(fmt3(sma3(i)), 5) + "  " + pad(fmt3(sma5(i)), 5)
        println(line)
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
