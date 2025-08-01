// Generated by Mochi v0.10.40 on 2025-07-26 00:01:18 GMT+7
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
    def isPrime(n: Int): Boolean = {
      if (n < 2) {
        return false
      }
      if (Math.floorMod(n, 2) == 0) {
        return n == 2
      }
      if (Math.floorMod(n, 3) == 0) {
        return n == 3
      }
      var d: Int = 5
      while ((d * d).asInstanceOf[Int] <= n) {
        if (Math.floorMod(n, d) == 0) {
          return false
        }
        d = (d + 2).asInstanceOf[Int]
        if (Math.floorMod(n, d) == 0) {
          return false
        }
        d = (d + 4).asInstanceOf[Int]
      }
      return true
    }
    def countPrimeFactors(_n: Int): Int = {
      var n: Int = _n
      if (n == 1) {
        return 0
      }
      if (isPrime(n)) {
        return 1
      }
      var count: Int = 0
      var f: Int = 2
      while (true) {
        if (Math.floorMod(n, f) == 0) {
          count = (count + 1).asInstanceOf[Int]
          n = (n / f).asInstanceOf[Int]
          if (n == 1) {
            return count
          }
          if (isPrime(n)) {
            f = n
          }
        } else {
          if (f >= 3) {
            f = (f + 2).asInstanceOf[Int]
          } else {
            f = 3
          }
        }
      }
      return count
    }
    def pad4(n: Int): String = {
      var s = String.valueOf(n)
      while ((s).size < 4) {
        s = " " + s
      }
      return s
    }
    def main(): Any = {
      val max: Int = 120
      println("The attractive numbers up to and including " + String.valueOf(max) + " are:")
      var count: Int = 0
      var line: String = ""
      var lineCount: Int = 0
      var i: Int = 1
      while (i <= max) {
        val c: Int = countPrimeFactors(i)
        if (isPrime(c)) {
          line = (line + pad4(i)).toString
          count = (count + 1).asInstanceOf[Int]
          lineCount = (lineCount + 1).asInstanceOf[Int]
          if (lineCount == 20) {
            println(line)
            line = ""
            lineCount = 0
          }
        }
        i = (i + 1).asInstanceOf[Int]
      }
      if (lineCount > 0) {
        println(line)
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
