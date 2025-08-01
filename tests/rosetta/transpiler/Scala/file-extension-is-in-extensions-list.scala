// Generated by Mochi v0.10.52 on 2025-08-02 02:09:38 GMT+7
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

def endsWith(s: String, suf: String): Boolean = {
  if ((s).size < (suf).size) {
    return false
  }
  return s.slice(((s).size - (suf).size).toInt, ((s).size).toInt) == suf
}

def lastIndexOf(s: String, sub: String): BigInt = {
  var idx: BigInt = BigInt(0) - BigInt(1)
  var i: BigInt = BigInt(0)
  while (i <= (s).size - (sub).size) {
    if (s.slice((i).toInt, (i + (sub).size).toInt) == sub) {
      idx = i
    }
    i = i + BigInt(1)
  }
  return idx
}

def fileExtInList(filename: String): ArrayBuffer[Any] = {
  val fl = filename.toLowerCase()
  for (ext <- extensions) {
    val ext2: String = "." + ext.toLowerCase()
    if (endsWith(fl, ext2)) {
      return ArrayBuffer(true, ext)
    }
  }
  var idx: BigInt = lastIndexOf(filename, ".")
  if (idx != BigInt(0) - BigInt(1)) {
    var t: String = filename.slice((idx + BigInt(1)).toInt, ((filename).size).toInt)
    if (t != "") {
      return ArrayBuffer(false, t)
    }
    return ArrayBuffer(false, "<empty>")
  }
  return ArrayBuffer(false, "<none>")
}

def pad(s: String, w: BigInt): String = {
  var t: String = s
  while ((t).size < w) {
    t = t + " "
  }
  return t
}

def main(): Any = {
  println("The listed extensions are:")
  println(extensions)
  val tests: ArrayBuffer[String] = ArrayBuffer("MyData.a##", "MyData.tar.Gz", "MyData.gzip", "MyData.7z.backup", "MyData...", "MyData", "MyData_v1.0.tar.bz2", "MyData_v1.0.bz2")
  for (t <- tests) {
    val res: ArrayBuffer[Any] = fileExtInList(t)
    val ok: Boolean = (res((BigInt(0)).toInt)).asInstanceOf[Boolean]
    val ext: String = (res((BigInt(1)).toInt)).toString
    println(pad(t, BigInt(20)) + " => " + String.valueOf(ok) + "  (extension = " + ext + ")")
  }
}

val extensions: ArrayBuffer[String] = ArrayBuffer("zip", "rar", "7z", "gz", "archive", "A##", "tar.bz2")

def main(args: Array[String]): Unit = {
  {
    System.gc()
    val _startMem = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val _start = _now()
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
