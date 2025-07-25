// Generated by Mochi v0.10.41 on 2025-07-26 17:21:35 GMT+7
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

case class Birthday(var month: BigInt, var day: BigInt)

def main(args: Array[String]): Unit = {
  {
    System.gc()
    val _startMem = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val _start = _now()
    def monthUnique(b: Birthday, list: ArrayBuffer[Birthday]): Boolean = {
      var c: BigInt = BigInt(0)
      for (x <- list) {
        if (x.month == b.month) {
          c = (c + BigInt(1)).asInstanceOf[BigInt]
        }
      }
      return c == BigInt(1)
    }
    def dayUnique(b: Birthday, list: ArrayBuffer[Birthday]): Boolean = {
      var c: BigInt = BigInt(0)
      for (x <- list) {
        if (x.day == b.day) {
          c = (c + BigInt(1)).asInstanceOf[BigInt]
        }
      }
      return c == BigInt(1)
    }
    def monthWithUniqueDay(b: Birthday, list: ArrayBuffer[Birthday]): Boolean = {
      for (x <- list) {
        if ((x.month == b.month && dayUnique(x.asInstanceOf[Birthday], list)).asInstanceOf[Boolean]) {
          return true
        }
      }
      return false
    }
    def bstr(b: Birthday): String = {
      val months: ArrayBuffer[String] = ArrayBuffer("", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
      return months((b.month).toInt) + " " + String.valueOf(b.day)
    }
    var choices: ArrayBuffer[Birthday] = ArrayBuffer(Birthday(BigInt(5), BigInt(15)), Birthday(BigInt(5), BigInt(16)), Birthday(BigInt(5), BigInt(19)), Birthday(BigInt(6), BigInt(17)), Birthday(BigInt(6), BigInt(18)), Birthday(BigInt(7), BigInt(14)), Birthday(BigInt(7), BigInt(16)), Birthday(BigInt(8), BigInt(14)), Birthday(BigInt(8), BigInt(15)), Birthday(BigInt(8), BigInt(17)))
    var filtered: ArrayBuffer[Birthday] = ArrayBuffer()
    for (bd <- choices) {
      if ((!monthUnique(bd.asInstanceOf[Birthday], choices)).asInstanceOf[Boolean]) {
        filtered = filtered :+ bd.asInstanceOf[Birthday]
      }
    }
    var filtered2: ArrayBuffer[Birthday] = ArrayBuffer()
    for (bd <- filtered) {
      if ((!monthWithUniqueDay(bd.asInstanceOf[Birthday], filtered)).asInstanceOf[Boolean]) {
        filtered2 = filtered2 :+ bd.asInstanceOf[Birthday]
      }
    }
    var filtered3: ArrayBuffer[Birthday] = ArrayBuffer()
    for (bd <- filtered2) {
      if (dayUnique(bd.asInstanceOf[Birthday], filtered2)) {
        filtered3 = filtered3 :+ bd.asInstanceOf[Birthday]
      }
    }
    var filtered4: ArrayBuffer[Birthday] = ArrayBuffer()
    for (bd <- filtered3) {
      if (monthUnique(bd.asInstanceOf[Birthday], filtered3)) {
        filtered4 = filtered4 :+ bd.asInstanceOf[Birthday]
      }
    }
    if ((filtered4).size == BigInt(1)) {
      println("Cheryl's birthday is " + bstr(filtered4((BigInt(0)).toInt)))
    } else {
      println("Something went wrong!")
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
