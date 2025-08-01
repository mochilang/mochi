// Generated by Mochi v0.10.40 on 2025-07-26 04:50:24 GMT+7
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

case class Colour(var R: Int, var G: Int, var B: Int)

case class Bitmap(var width: Int, var height: Int, var pixels: ArrayBuffer[ArrayBuffer[Colour]])

def main(args: Array[String]): Unit = {
  {
    System.gc()
    val _startMem = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val _start = _now()
    def newBitmap(w: Int, h: Int, c: Colour): Bitmap = {
      var rows: ArrayBuffer[ArrayBuffer[Colour]] = ArrayBuffer()
      var y: Int = 0
      while (y < h) {
        var row: ArrayBuffer[Colour] = ArrayBuffer()
        var x: Int = 0
        while (x < w) {
          row = row :+ c
          x = (x + 1).asInstanceOf[Int]
        }
        rows = rows :+ row
        y = (y + 1).asInstanceOf[Int]
      }
      return Bitmap(w, h, rows)
    }
    def setPixel(b: Bitmap, x: Int, y: Int, c: Colour): Any = {
      var rows: ArrayBuffer[ArrayBuffer[Colour]] = b.pixels
      var row: ArrayBuffer[Colour] = rows(y)
      row(x) = c
      rows(y) = row
      b.pixels = rows
    }
    def fillRect(b: Bitmap, x: Int, y: Int, w: Int, h: Int, c: Colour): Any = {
      var yy: Int = y
      while (yy < (y + h).asInstanceOf[Int]) {
        var xx: Int = x
        while (xx < (x + w).asInstanceOf[Int]) {
          setPixel(b, xx, yy, c)
          xx = (xx + 1).asInstanceOf[Int]
        }
        yy = (yy + 1).asInstanceOf[Int]
      }
    }
    def pad(n: Int, width: Int): String = {
      var s = String.valueOf(n)
      while ((s).size < width) {
        s = " " + s
      }
      return s
    }
    def writePPMP3(b: Bitmap): String = {
      var maxv: Int = 0
      var y: Int = 0
      while (y < b.height) {
        var x: Int = 0
        while (x < b.width) {
          val p: Colour = b.pixels(y)(x)
          if (p.R > maxv) {
            maxv = p.R
          }
          if (p.G > maxv) {
            maxv = p.G
          }
          if (p.B > maxv) {
            maxv = p.B
          }
          x = (x + 1).asInstanceOf[Int]
        }
        y = (y + 1).asInstanceOf[Int]
      }
      var out: String = "P3\n# generated from Bitmap.writeppmp3\n" + String.valueOf(b.width) + " " + String.valueOf(b.height) + "\n" + String.valueOf(maxv) + "\n"
      var numsize: Int = (String.valueOf(maxv)).size
      y = (b.height - 1).asInstanceOf[Int]
      while (y >= 0) {
        var line: String = ""
        var x: Int = 0
        while (x < b.width) {
          val p: Colour = b.pixels(y)(x)
          line = line + "   " + pad(p.R, numsize) + " " + pad(p.G, numsize) + " " + pad(p.B, numsize)
          x = (x + 1).asInstanceOf[Int]
        }
        out = (out + line).toString
        if (y > 0) {
          out = out + "\n"
        } else {
          out = out + "\n"
        }
        y = (y - 1).asInstanceOf[Int]
      }
      return out
    }
    def main(): Any = {
      val black: Colour = Colour(0, 0, 0)
      val white: Colour = Colour(255, 255, 255)
      var bm: Bitmap = newBitmap(4, 4, black)
      fillRect(bm, 1, 0, 1, 2, white)
      setPixel(bm, 3, 3, Colour(127, 0, 63))
      val ppm: String = writePPMP3(bm)
      println(ppm)
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
