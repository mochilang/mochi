// Generated by Mochi v0.10.42 on 2025-07-28 00:31:54 GMT+7
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
    def removeName(names: ArrayBuffer[String], name: String): ArrayBuffer[String] = {
      var out: ArrayBuffer[String] = ArrayBuffer()
      for (n <- names) {
        if (n != name) {
          out = out :+ n
        }
      }
      return out
    }
    def main(): Any = {
      var clients: ArrayBuffer[String] = ArrayBuffer()
      def broadcast(msg: String): Any = {
        println(msg)
      }
      def add(name: String): Any = {
        clients = clients :+ name
        broadcast("+++ \"" + name + "\" connected +++\n")
      }
      def send(name: String, msg: String): Any = {
        broadcast(name + "> " + msg + "\n")
      }
      def remove(name: String): Any = {
        clients = removeName(clients, name)
        broadcast("--- \"" + name + "\" disconnected ---\n")
      }
      add("Alice")
      add("Bob")
      send("Alice", "Hello Bob!")
      send("Bob", "Hi Alice!")
      remove("Bob")
      remove("Alice")
      broadcast("Server stopping!\n")
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
