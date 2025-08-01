// Generated by Mochi v0.10.42 on 2025-07-27 17:28:10 GMT+7
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

case class Field(var s: ArrayBuffer[ArrayBuffer[Boolean]], var w: BigInt, var h: BigInt)

case class Life(var a: Field, var b: Field, var w: BigInt, var h: BigInt)

def main(args: Array[String]): Unit = {
  {
    System.gc()
    val _startMem = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val _start = _now()
    var seed: BigInt = BigInt(1)
    def randN(n: BigInt): BigInt = {
      seed = ((seed * BigInt(1664525) + BigInt(1013904223)) % BigInt(2147483647)).asInstanceOf[BigInt]
      return (seed % n).asInstanceOf[BigInt]
    }
    def newField(w: BigInt, h: BigInt): Field = {
      var rows: ArrayBuffer[ArrayBuffer[Boolean]] = ArrayBuffer()
      var y: BigInt = BigInt(0)
      while (y < h) {
        var row: ArrayBuffer[Boolean] = ArrayBuffer()
        var x: BigInt = BigInt(0)
        while (x < w) {
          row = row :+ false
          x = (x + BigInt(1)).asInstanceOf[BigInt]
        }
        rows = rows :+ row
        y = (y + BigInt(1)).asInstanceOf[BigInt]
      }
      return Field(rows, w, h)
    }
    def setCell(f: Field, x: BigInt, y: BigInt, b: Boolean): Any = {
      var rows: ArrayBuffer[ArrayBuffer[Boolean]] = f.s
      var row: ArrayBuffer[Boolean] = rows((y).toInt)
      row((x).toInt) = b
      rows((y).toInt) = row
      f.s = rows
    }
    def state(f: Field, _x: BigInt, _y: BigInt): Boolean = {
      var x: BigInt = _x
      var y: BigInt = _y
      while (y < BigInt(0)) {
        y = (y + f.h).asInstanceOf[BigInt]
      }
      while (x < BigInt(0)) {
        x = (x + f.w).asInstanceOf[BigInt]
      }
      return f.s((y % f.h).toInt)((x % f.w).toInt)
    }
    def nextState(f: Field, x: BigInt, y: BigInt): Boolean = {
      var count: BigInt = BigInt(0)
      var dy: BigInt = BigInt(0) - BigInt(1)
      while (dy <= BigInt(1)) {
        var dx: BigInt = BigInt(0) - BigInt(1)
        while (dx <= BigInt(1)) {
          if (((!(dx == BigInt(0) && dy == BigInt(0)).asInstanceOf[Boolean]).asInstanceOf[Boolean] && state(f, (x + dx).asInstanceOf[BigInt], (y + dy).asInstanceOf[BigInt])).asInstanceOf[Boolean]) {
            count = (count + BigInt(1)).asInstanceOf[BigInt]
          }
          dx = (dx + BigInt(1)).asInstanceOf[BigInt]
        }
        dy = (dy + BigInt(1)).asInstanceOf[BigInt]
      }
      return (count == BigInt(3) || (count == BigInt(2) && state(f, x, y)).asInstanceOf[Boolean]).asInstanceOf[Boolean]
    }
    def newLife(w: BigInt, h: BigInt): Life = {
      var a: Field = newField(w, h)
      var i: BigInt = BigInt(0)
      while (i < w * h / BigInt(2)) {
        setCell(a, randN(w), randN(h), true)
        i = (i + BigInt(1)).asInstanceOf[BigInt]
      }
      return Life(a, newField(w, h), w, h)
    }
    def step(l: Life): Any = {
      var y: BigInt = BigInt(0)
      while (y < l.h) {
        var x: BigInt = BigInt(0)
        while (x < l.w) {
          setCell(l.b, x, y, nextState(l.a, x, y))
          x = (x + BigInt(1)).asInstanceOf[BigInt]
        }
        y = (y + BigInt(1)).asInstanceOf[BigInt]
      }
      var tmp: Field = l.a
      l.a = l.b
      l.b = tmp
    }
    def lifeString(l: Life): String = {
      var out: String = ""
      var y: BigInt = BigInt(0)
      while (y < l.h) {
        var x: BigInt = BigInt(0)
        while (x < l.w) {
          if (state(l.a, x, y)) {
            out = out + "*"
          } else {
            out = out + " "
          }
          x = (x + BigInt(1)).asInstanceOf[BigInt]
        }
        out = out + "\n"
        y = (y + BigInt(1)).asInstanceOf[BigInt]
      }
      return out
    }
    def main(): Any = {
      var l: Life = newLife(BigInt(80), BigInt(15))
      var i: BigInt = BigInt(0)
      while (i < BigInt(300)) {
        step(l)
        println("\f")
        println(lifeString(l))
        i = (i + BigInt(1)).asInstanceOf[BigInt]
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
