// Generated by Mochi v0.10.55 on 2025-08-02 21:08:10 GMT+7
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

def bitAt(x: BigInt, idx: BigInt): BigInt = {
  var v: BigInt = x
  var i: BigInt = BigInt(0)
  while (i < idx) {
    v = v / BigInt(2)
    i = i + BigInt(1)
  }
  return v % BigInt(2)
}

def outputState(state: String): Any = {
  var line: String = ""
  var i: BigInt = BigInt(0)
  while (i < BigInt(((state).size).toInt)) {
    if (state.slice((i).toInt, (i + BigInt(1)).toInt) == "1") {
      line = line + "#"
    } else {
      line = line + " "
    }
    i = i + BigInt(1)
  }
  println(line)
}

def step(state: String, r: BigInt): String = {
  val cells: Int = (state).size
  var out: String = ""
  var i: BigInt = BigInt(0)
  while (i < BigInt(cells.toString.toDouble.toInt)) {
    val l: String = state.slice(((i - BigInt(1) + cells) % cells).toInt, ((i - BigInt(1) + cells) % cells + BigInt(1)).toInt)
    val c: String = state.slice((i).toInt, (i + BigInt(1)).toInt)
    val rt: String = state.slice(((i + BigInt(1)) % cells).toInt, ((i + BigInt(1)) % cells + BigInt(1)).toInt)
    var idx: BigInt = BigInt(0)
    if (l == "1") {
      idx = idx + BigInt(4)
    }
    if (c == "1") {
      idx = idx + BigInt(2)
    }
    if (rt == "1") {
      idx = idx + BigInt(1)
    }
    if (bitAt(r, idx) == BigInt(1)) {
      out = out + "1"
    } else {
      out = out + "0"
    }
    i = i + BigInt(1)
  }
  return out
}

def elem(r: BigInt, cells: BigInt, generations: BigInt, state: String): Any = {
  outputState(state)
  var g: BigInt = BigInt(0)
  var s: String = state
  while (g < generations) {
    s = step(s, r)
    outputState(s)
    g = g + BigInt(1)
  }
}

def randInit(cells: BigInt, seed: BigInt): String = {
  var s: String = ""
  var `val`: BigInt = seed
  var i: BigInt = BigInt(0)
  while (i < cells) {
    `val` = (`val` * BigInt(1664525) + BigInt(1013904223)) % BigInt(2147483647)
    if (`val` % BigInt(2) == BigInt(0)) {
      s = s + "0"
    } else {
      s = s + "1"
    }
    i = i + BigInt(1)
  }
  return s
}

def singleInit(cells: BigInt): String = {
  var s: String = ""
  var i: BigInt = BigInt(0)
  while (i < cells) {
    if (i == cells / BigInt(2)) {
      s = s + "1"
    } else {
      s = s + "0"
    }
    i = i + BigInt(1)
  }
  return s
}

def main(): Any = {
  val cells: BigInt = BigInt(20)
  val generations: BigInt = BigInt(9)
  println("Single 1, rule 90:")
  var state: String = singleInit(cells)
  elem(BigInt(90), cells, generations, state)
  println("Random intial state, rule 30:")
  state = randInit(cells, BigInt(3))
  elem(BigInt(30), cells, generations, state)
}

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
