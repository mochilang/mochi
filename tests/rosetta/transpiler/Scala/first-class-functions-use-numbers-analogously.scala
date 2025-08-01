// Generated by Mochi v0.10.50 on 2025-07-30 21:21:27 GMT+7
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
    def multiplier(n1: Double, n2: Double): (Double) => Double = {
      val n1n2 = n1 * n2
      return (((m: Double) => n1n2 * m)).asInstanceOf[(Double) => Double]
    }
    def main(): Any = {
      val x: Double = 2.0
      val xi: Double = 0.5
      val y: Double = 4.0
      val yi: Double = 0.25
      val z = x + y
      val zi: Double = 1.0 / (x + y)
      val numbers: ArrayBuffer[Double] = ArrayBuffer(x, y, z)
      val inverses: ArrayBuffer[Double] = ArrayBuffer(xi, yi, zi)
      var mfs: ArrayBuffer[(Double) => Double] = ArrayBuffer()
      var i: BigInt = BigInt(0)
      while (i < (numbers).size) {
        mfs = mfs :+ multiplier(numbers((i).toInt), inverses((i).toInt))
        i = (i + BigInt(1)).asInstanceOf[BigInt]
      }
      for (mf <- mfs) {
        println(String.valueOf(mf(1.0)))
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
