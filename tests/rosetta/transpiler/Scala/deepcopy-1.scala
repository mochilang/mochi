// Generated by Mochi v0.10.52 on 2025-08-01 19:13:41 GMT+7
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

case class cds(var i: BigInt, var s: String, var b: ArrayBuffer[BigInt], var m: scala.collection.mutable.Map[BigInt,Boolean])

def main(args: Array[String]): Unit = {
  {
    System.gc()
    val _startMem = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val _start = _now()
    def copyList(src: ArrayBuffer[BigInt]): ArrayBuffer[BigInt] = {
      var out: ArrayBuffer[BigInt] = ArrayBuffer()
      for (v <- src) {
        out = out :+ v
      }
      return out
    }
    def copyMap(src: scala.collection.mutable.Map[BigInt,Boolean]): scala.collection.mutable.Map[BigInt,Boolean] = {
      var out: scala.collection.mutable.Map[BigInt,Boolean] = scala.collection.mutable.Map()
      for (k <- src.keys) {
        out.update(k, src(k))
      }
      return out
    }
    def deepcopy(c: cds): cds = {
      return cds(c.i, c.s, copyList(c.b), copyMap(c.m))
    }
    def cdsStr(c: cds): String = {
      var bs: String = "["
      var i: BigInt = BigInt(0)
      while (i < (c.b).size) {
        bs = bs + String.valueOf(c.b((i).toInt))
        if (i < (c.b).size - BigInt(1).toInt) {
          bs = bs + " "
        }
        i = i + BigInt(1)
      }
      bs = bs + "]"
      var ms: String = "map["
      var first: Boolean = true
      for (k <- c.m.keys) {
        if ((!first).asInstanceOf[Boolean]) {
          ms = ms + " "
        }
        ms = ms + String.valueOf(k) + ":" + String.valueOf(c.m(k))
        first = false
      }
      ms = ms + "]"
      return "{" + String.valueOf(c.i) + " " + c.s + " " + bs + " " + ms + "}"
    }
    var c1: cds = cds(BigInt(1), "one", ArrayBuffer(BigInt(117), BigInt(110), BigInt(105), BigInt(116)), scala.collection.mutable.Map(BigInt(1) -> (true)))
    var c2: cds = deepcopy(c1)
    println(cdsStr(c1))
    println(cdsStr(c2))
    c1 = cds(BigInt(0), "nil", ArrayBuffer(BigInt(122), BigInt(101), BigInt(114), BigInt(111)), scala.collection.mutable.Map(BigInt(1) -> (false)))
    println(cdsStr(c1))
    println(cdsStr(c2))
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
