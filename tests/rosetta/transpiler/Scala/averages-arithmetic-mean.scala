// Generated by Mochi v0.10.40 on 2025-07-26 00:01:24 GMT+7
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
    def mean(v: ArrayBuffer[Double]): Map[String,Any] = {
      if ((v).size == 0) {
        return (Map("ok" -> (false))).asInstanceOf[Map[String,Any]]
      }
      var sum: Double = 0.0
      var i: Int = 0
      while (i < (v).size) {
        sum = (sum + v(i)).toString.toDouble
        i = (i + 1).asInstanceOf[Int]
      }
      return Map("ok" -> (true), "mean" -> (sum / ((v).size).toString.toDouble))
    }
    def main(): Any = {
      val sets = ArrayBuffer(ArrayBuffer(), ArrayBuffer(3.0, 1.0, 4.0, 1.0, 5.0, 9.0), ArrayBuffer(100000000000000000000.0, 3.0, 1.0, 4.0, 1.0, 5.0, 9.0, 0 - 100000000000000000000.0), ArrayBuffer(10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.11), ArrayBuffer(10.0, 20.0, 30.0, 40.0, 50.0, 0 - 100.0, 4.7, 0 - 1100.0))
      for (v <- sets) {
        println("Vector: " + String.valueOf(v))
        val r: Map[String,Any] = mean(v.asInstanceOf[ArrayBuffer[Double]])
        if ((r.getOrElse("ok", null.asInstanceOf[Any])).asInstanceOf[Boolean]) {
          println("Mean of " + String.valueOf((v).size) + " numbers is " + String.valueOf(r.getOrElse("mean", null.asInstanceOf[Any])))
        } else {
          println("Mean undefined")
        }
        println("")
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
