// Generated by Mochi v0.10.42 on 2025-07-27 22:16:55 GMT+7
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.math.BigInt
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
    def padLeft(n: BigInt, width: BigInt): String = {
      var s = String.valueOf(n)
      while ((s).size < width) {
        s = " " + s
      }
      return s
    }
    def modPow(base: BigInt, exp: BigInt, mod: BigInt): BigInt = {
      var result = BigInt(1) % mod
      var b = base % mod
      var e: BigInt = exp
      while (e > BigInt(0)) {
        if (e % BigInt(2) == BigInt(1)) {
          result = (result * b % mod).asInstanceOf[BigInt]
        }
        b = (b * b % mod).asInstanceOf[BigInt]
        e = (e / BigInt(2)).asInstanceOf[BigInt]
      }
      return result
    }
    def main(): Any = {
      var k: BigInt = BigInt(2)
      val _br2 = new Breaks
      _br2.breakable {
        while (k <= BigInt(10)) {
          println("The first 50 Curzon numbers using a base of " + String.valueOf(k) + " :")
          var count: BigInt = BigInt(0)
          var n: BigInt = BigInt(1)
          var curzon50: ArrayBuffer[BigInt] = ArrayBuffer()
          val _br3 = new Breaks
          _br3.breakable {
            while (true) {
              val d = k * n + BigInt(1)
              if ((modPow(k, n, d) + BigInt(1)) % d == BigInt(0)) {
                if (count < BigInt(50)) {
                  curzon50 = curzon50 :+ n
                }
                count = (count + BigInt(1)).asInstanceOf[BigInt]
                if (count == BigInt(50)) {
                  var idx: BigInt = BigInt(0)
                  while (idx < (curzon50).size) {
                    var line: String = ""
                    var j: BigInt = BigInt(0)
                    while (j < BigInt(10)) {
                      line = line + padLeft(curzon50((idx).toInt), BigInt(4)) + " "
                      idx = (idx + BigInt(1)).asInstanceOf[BigInt]
                      j = (j + BigInt(1)).asInstanceOf[BigInt]
                    }
                    println(line.slice((BigInt(0)).toInt, ((line).size - BigInt(1)).toInt))
                  }
                }
                if (count == BigInt(1000)) {
                  println("\nOne thousandth: " + String.valueOf(n))
                  _br3.break()
                }
              }
              n = (n + BigInt(1)).asInstanceOf[BigInt]
            }
          }
          println("")
          k = (k + BigInt(2)).asInstanceOf[BigInt]
        }
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
