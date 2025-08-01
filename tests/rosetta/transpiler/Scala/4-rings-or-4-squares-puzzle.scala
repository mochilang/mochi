// Generated by Mochi v0.10.40 on 2025-07-25 19:17:59 GMT+7
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.collection.immutable.ListMap
import scala.util.control.Breaks
import scala.util.control.Breaks._
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
    def validComb(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int): Boolean = {
      val square1 = a + b
      val square2 = b + c + d
      val square3 = d + e + f
      val square4 = f + g
      return ((square1 == square2 && square2 == square3).asInstanceOf[Boolean] && square3 == square4).asInstanceOf[Boolean]
    }
    def isUnique(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int): Boolean = {
      var nums: ArrayBuffer[Int] = ArrayBuffer(a, b, c, d, e, f, g)
      var i: Int = 0
      while (i < (nums).size) {
        var j = i + 1
        while (j < (nums).size) {
          if (nums(i) == nums(j)) {
            return false
          }
          j = (j + 1).asInstanceOf[Int]
        }
        i = (i + 1).asInstanceOf[Int]
      }
      return true
    }
    def getCombs(low: Int, high: Int, unique: Boolean): Map[String,Any] = {
      var valid: ArrayBuffer[Any] = ArrayBuffer()
      var count: Int = 0
      val _ct2 = new Breaks
      for (b <- low until high + 1) {
        _ct2.breakable {
          val _ct3 = new Breaks
          for (c <- low until high + 1) {
            _ct3.breakable {
              val _ct4 = new Breaks
              for (d <- low until high + 1) {
                _ct4.breakable {
                  val s = b.asInstanceOf[Int] + c.asInstanceOf[Int] + d
                  val _ct5 = new Breaks
                  for (e <- low until high + 1) {
                    _ct5.breakable {
                      val _ct6 = new Breaks
                      for (f <- low until high + 1) {
                        _ct6.breakable {
                          val a: Int = s.asInstanceOf[Int] - b.asInstanceOf[Int]
                          val g: Int = s.asInstanceOf[Int] - f.asInstanceOf[Int]
                          if ((a < low || a > high).asInstanceOf[Boolean]) {
                            _ct6.break()
                          }
                          if ((g < low || g > high).asInstanceOf[Boolean]) {
                            _ct6.break()
                          }
                          if (d.asInstanceOf[Int] + e.asInstanceOf[Int] + f != s) {
                            _ct6.break()
                          }
                          if (f + g != s) {
                            _ct6.break()
                          }
                          if (((!unique).asInstanceOf[Boolean] || isUnique(a, b.asInstanceOf[Int], c.asInstanceOf[Int], d.asInstanceOf[Int], e.asInstanceOf[Int], f.asInstanceOf[Int], g)).asInstanceOf[Boolean]) {
                            valid = valid :+ ArrayBuffer(a, b, c, d, e, f, g)
                            count = (count + 1).asInstanceOf[Int]
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      return Map("count" -> (count), "list" -> (valid))
    }
    val r1: Map[String,Any] = getCombs(1, 7, true)
    println(String.valueOf(r1.getOrElse("count", null.asInstanceOf[Any])) + " unique solutions in 1 to 7")
    println(r1.getOrElse("list", null.asInstanceOf[Any]))
    val r2: Map[String,Any] = getCombs(3, 9, true)
    println(String.valueOf(r2.getOrElse("count", null.asInstanceOf[Any])) + " unique solutions in 3 to 9")
    println(r2.getOrElse("list", null.asInstanceOf[Any]))
    val r3: Map[String,Any] = getCombs(0, 9, false)
    println(String.valueOf(r3.getOrElse("count", null.asInstanceOf[Any])) + " non-unique solutions in 0 to 9")
    val _end = _now()
    System.gc()
    val _endMem = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val _durUs = (_end - _start) / 1000
    var _memDiff = _endMem - _startMem
    if (_memDiff <= 0) _memDiff = _endMem
    println(toJson(Map("duration_us" -> _durUs, "memory_bytes" -> _memDiff, "name" -> "main")))
  }
}
}
