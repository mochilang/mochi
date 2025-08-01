// Generated by Mochi v0.10.40 on 2025-07-26 04:50:46 GMT+7
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
    def padLeft(s: String, w: Int): String = {
      var res: String = ""
      var n = w - (s).size
      while (n > 0) {
        res = res + " "
        n = (n - 1).asInstanceOf[Int]
      }
      return (res + s).toString
    }
    def padRight(s: String, w: Int): String = {
      var out: String = s
      var i: Int = (s).size
      while (i < w) {
        out = out + " "
        i = (i + 1).asInstanceOf[Int]
      }
      return out
    }
    def indexOf(s: String, ch: String): Int = {
      var i: Int = 0
      while (i < (s).size) {
        if (s.slice(i, i + 1) == ch) {
          return i
        }
        i = (i + 1).asInstanceOf[Int]
      }
      return 0 - 1
    }
    def format2(f: Double): String = {
      var s = String.valueOf(f)
      val idx: Int = indexOf(s, ".")
      if (idx < 0) {
        s = s + ".00"
      } else {
        var need = idx + 3
        if ((s).size > need) {
          s = s.slice(0, need)
        } else {
          while ((s).size < need) {
            s = s + "0"
          }
        }
      }
      return s
    }
    def cpx(h: Double): Int = {
      var x: Int = (h / 11.25 + 0.5).asInstanceOf[Int]
      x = (Math.floorMod(x, 32)).asInstanceOf[Int]
      if (x < 0) {
        x = (x + 32).asInstanceOf[Int]
      }
      return x
    }
    val compassPoint: ArrayBuffer[String] = ArrayBuffer("North", "North by east", "North-northeast", "Northeast by north", "Northeast", "Northeast by east", "East-northeast", "East by north", "East", "East by south", "East-southeast", "Southeast by east", "Southeast", "Southeast by south", "South-southeast", "South by east", "South", "South by west", "South-southwest", "Southwest by south", "Southwest", "Southwest by west", "West-southwest", "West by south", "West", "West by north", "West-northwest", "Northwest by west", "Northwest", "Northwest by north", "North-northwest", "North by west")
    def degrees2compasspoint(h: Double): String = {
      return compassPoint(cpx(h))
    }
    val headings: ArrayBuffer[Double] = ArrayBuffer(0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38)
    println("Index  Compass point         Degree")
    var i: Int = 0
    while (i < (headings).size) {
      val h: Double = headings(i)
      val idx = Math.floorMod(i, 32) + 1
      val cp: String = degrees2compasspoint(h)
      println(padLeft((String.valueOf(idx)).toString, 4) + "   " + padRight(cp, 19) + " " + format2(h) + "\u00b0")
      i = (i + 1).asInstanceOf[Int]
    }
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
