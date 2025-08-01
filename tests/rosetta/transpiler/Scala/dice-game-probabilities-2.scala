// Generated by Mochi v0.10.52 on 2025-08-01 19:06:18 GMT+7
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
    def roll(nDice: BigInt, nSides: BigInt): BigInt = {
      var sum: BigInt = BigInt(0)
      var i: BigInt = BigInt(0)
      while (i < nDice) {
        sum = sum + _now() % nSides + BigInt(1)
        i = i + BigInt(1)
      }
      return sum
    }
    def beats(n1: BigInt, s1: BigInt, n2: BigInt, s2: BigInt, trials: BigInt): Double = {
      var wins: BigInt = BigInt(0)
      var i: BigInt = BigInt(0)
      while (i < trials) {
        if (roll(n1, s1) > roll(n2, s2)) {
          wins = wins + BigInt(1)
        }
        i = i + BigInt(1)
      }
      return wins.toString.toDouble / trials.toString.toDouble
    }
    println(String.valueOf(beats(BigInt(9), BigInt(4), BigInt(6), BigInt(6), BigInt(1000))))
    println(String.valueOf(beats(BigInt(5), BigInt(10), BigInt(7), BigInt(6), BigInt(1000))))
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
