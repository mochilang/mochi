// Generated by Mochi v0.10.40 on 2025-07-26 00:01:27 GMT+7
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
    val PI: Double = 3.141592653589793
    def sinApprox(x: Double): Double = {
      var term: Double = x
      var sum: Double = x
      var n: Int = 1
      while (n <= 8) {
        val denom: Double = ((2 * n).toString.toDouble * (2 * n + 1).toString.toDouble).toString.toDouble
        term = ((0 - term) * x * x / denom).toString.toDouble
        sum = (sum + term).toString.toDouble
        n = (n + 1).asInstanceOf[Int]
      }
      return sum
    }
    def cosApprox(x: Double): Double = {
      var term: Double = 1.0
      var sum: Double = 1.0
      var n: Int = 1
      while (n <= 8) {
        val denom: Double = ((2 * n - 1).toString.toDouble * (2 * n).toString.toDouble).toString.toDouble
        term = ((0 - term) * x * x / denom).toString.toDouble
        sum = (sum + term).toString.toDouble
        n = (n + 1).asInstanceOf[Int]
      }
      return sum
    }
    def atanApprox(x: Double): Double = {
      if (x > 1.0) {
        return PI / 2.0 - x / (x * x + 0.28)
      }
      if (x < 0 - 1.0) {
        return (0 - PI) / 2.0 - x / (x * x + 0.28)
      }
      return x / (1.0 + 0.28 * x * x)
    }
    def atan2Approx(y: Double, x: Double): Double = {
      if (x > 0.0) {
        return atanApprox((y / x).toString.toDouble)
      }
      if (x < 0.0) {
        if (y >= 0.0) {
          return (atanApprox((y / x).toString.toDouble) + PI).toString.toDouble
        }
        return (atanApprox((y / x).toString.toDouble) - PI).toString.toDouble
      }
      if (y > 0.0) {
        return PI / 2.0
      }
      if (y < 0.0) {
        return (0 - PI) / 2.0
      }
      return 0.0
    }
    def digit(ch: String): Int = {
      val digits: String = "0123456789"
      var i: Int = 0
      while (i < (digits).size) {
        if (digits.substring(i, i + 1) == ch) {
          return i
        }
        i = (i + 1).asInstanceOf[Int]
      }
      return 0
    }
    def parseTwo(s: String, idx: Int): Int = {
      return (digit(s.substring(idx, idx + 1)) * 10 + digit(s.substring(idx + 1, idx + 2))).asInstanceOf[Int]
    }
    def parseSec(s: String): Double = {
      val h: Int = parseTwo(s, 0)
      val m: Int = parseTwo(s, 3)
      val sec: Int = parseTwo(s, 6)
      val tmp = (h * 60 + m) * 60 + sec
      return tmp.toString.toDouble
    }
    def pad(n: Int): String = {
      if (n < 10) {
        return "0" + String.valueOf(n)
      }
      return (String.valueOf(n)).toString
    }
    def meanTime(times: ArrayBuffer[String]): String = {
      var ssum: Double = 0.0
      var csum: Double = 0.0
      var i: Int = 0
      while (i < (times).size) {
        val sec: Double = parseSec(times(i))
        val ang: Double = sec * 2.0 * PI / 86400.0
        ssum = (ssum + sinApprox(ang)).toString.toDouble
        csum = (csum + cosApprox(ang)).toString.toDouble
        i = (i + 1).asInstanceOf[Int]
      }
      var theta: Double = atan2Approx(ssum, csum)
      var frac: Double = theta / 2.0 * PI
      while (frac < 0.0) {
        frac = frac + 1.0
      }
      val total: Double = frac * 86400.0
      val si: Int = total.asInstanceOf[Int]
      val h: Int = (si / 3600).asInstanceOf[Int]
      val m: Int = (Math.floorMod(si, 3600) / 60).asInstanceOf[Int]
      val s: Int = (Math.floorMod(si, 60)).asInstanceOf[Int]
      return pad(h) + ":" + pad(m) + ":" + pad(s)
    }
    def main(): Any = {
      val inputs: ArrayBuffer[String] = ArrayBuffer("23:00:17", "23:40:20", "00:12:45", "00:17:19")
      println(meanTime(inputs))
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
