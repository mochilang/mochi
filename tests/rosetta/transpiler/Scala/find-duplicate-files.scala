// Generated by Mochi v0.10.50 on 2025-07-30 21:20:32 GMT+7
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
    def findDuplicates(fs: scala.collection.mutable.Map[String,String], paths: ArrayBuffer[String]): ArrayBuffer[ArrayBuffer[String]] = {
      var seen: scala.collection.mutable.Map[String,String] = scala.collection.mutable.Map()
      var dups: ArrayBuffer[ArrayBuffer[String]] = ArrayBuffer()
      for (path <- paths) {
        val content: String = fs.getOrElse(path, null.asInstanceOf[String])
        if ((seen.contains(content)).asInstanceOf[Boolean]) {
          dups = dups :+ (ArrayBuffer(seen.getOrElse(content, null.asInstanceOf[String]), path)).asInstanceOf[ArrayBuffer[String]]
        } else {
          seen.update(content, path)
        }
      }
      return dups
    }
    def main(): Any = {
      var fs: scala.collection.mutable.Map[String,String] = scala.collection.mutable.Map("a.txt" -> ("hello"), "b.txt" -> ("world"), "c.txt" -> ("hello"), "d.txt" -> ("foo"), "e.txt" -> ("world"))
      val paths: ArrayBuffer[String] = ArrayBuffer("a.txt", "b.txt", "c.txt", "d.txt", "e.txt")
      val dups: ArrayBuffer[ArrayBuffer[String]] = findDuplicates(fs, paths)
      for (pair <- dups) {
        println(pair((BigInt(0)).toInt) + " <==> " + pair((BigInt(1)).toInt))
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
