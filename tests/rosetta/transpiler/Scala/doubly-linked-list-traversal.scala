// Generated by Mochi v0.10.52 on 2025-08-01 19:21:41 GMT+7
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

def listString(): String = {
  if (head == BigInt(0) - BigInt(1)) {
    return "<nil>"
  }
  var r: String = "[" + nodes(head).asInstanceOf[scala.collection.mutable.Map[String,Any]]("value")
  var id: BigInt = nodes(head).asInstanceOf[scala.collection.mutable.Map[String,Any]]("next")
  while (id != BigInt(0) - BigInt(1)) {
    r = r + " " + nodes(id).asInstanceOf[scala.collection.mutable.Map[String,Any]]("value")
    id = nodes(id).asInstanceOf[scala.collection.mutable.Map[String,Any]]("next")
  }
  r = r + "]"
  return r
}

var nodes: scala.collection.mutable.Map[BigInt,scala.collection.mutable.Map[String,Any]] = scala.collection.mutable.Map()

var head: BigInt = BigInt(0) - BigInt(1)

var tail: BigInt = BigInt(0) - BigInt(1)

var out: String = "From tail:"

var id: BigInt = tail

def main(args: Array[String]): Unit = {
  {
    System.gc()
    val _startMem = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val _start = _now()
    println(listString())
    nodes.update(BigInt(0), scala.collection.mutable.Map("value" -> ("A"), "next" -> (BigInt(0) - BigInt(1)), "prev" -> (BigInt(0) - BigInt(1))))
    head = BigInt(0)
    tail = BigInt(0)
    nodes.update(BigInt(1), scala.collection.mutable.Map("value" -> ("B"), "next" -> (BigInt(0) - BigInt(1)), "prev" -> (BigInt(0))))
    nodes(BigInt(0)).update("next", BigInt(1))
    tail = BigInt(1)
    println(listString())
    nodes.update(BigInt(2), scala.collection.mutable.Map("value" -> ("C"), "next" -> (BigInt(1)), "prev" -> (BigInt(0))))
    nodes(BigInt(1)).update("prev", BigInt(2))
    nodes(BigInt(0)).update("next", BigInt(2))
    println(listString())
    while (id != BigInt(0) - BigInt(1)) {
      out = out + " " + nodes(id).asInstanceOf[scala.collection.mutable.Map[String,Any]]("value")
      id = nodes(id).asInstanceOf[scala.collection.mutable.Map[String,Any]]("prev")
    }
    println(out)
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
