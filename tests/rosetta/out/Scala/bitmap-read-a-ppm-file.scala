case class Pixel(var R: Int, var G: Int, var B: Int)

case class Bitmap(var w: Int, var h: Int, var max: Int, var data: List[List[Pixel]])

object bitmap_read_a_ppm_file {
  def newBitmap(w: Int, h: Int, max: Int): Bitmap = {
    var rows: List[List[Pixel]] = scala.collection.mutable.ArrayBuffer[Any]()
    var y = 0
    while (y < h) {
      var row: List[Pixel] = scala.collection.mutable.ArrayBuffer[Any]()
      var x = 0
      while (x < w) {
        row = row :+ Pixel(R = 0, G = 0, B = 0)
        x += 1
      }
      rows = rows :+ row
      y += 1
    }
    return Bitmap(w = w, h = h, max = max, data = rows)
  }
  
  def setPx(b: Bitmap, x: Int, y: Int, p: Pixel) = {
    var rows = b.data
    var row = (rows).apply(y)
    row(x) = p
    rows(y) = row
    b.data = rows
  }
  
  def getPx(b: Bitmap, x: Int, y: Int): Pixel = ((b.data).apply(y)).apply(x)
  
  def splitLines(s: String): List[String] = {
    var out: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var cur = ""
    var i = 0
    while (i < s.length) {
      val ch = substr(s, i, i + 1)
      if (ch == "\n") {
        out = out :+ cur
        cur = ""
      } else {
        cur += ch
      }
      i += 1
    }
    out = out :+ cur
    return out
  }
  
  def splitWS(s: String): List[String] = {
    var out: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var cur = ""
    var i = 0
    while (i < s.length) {
      val ch = substr(s, i, i + 1)
      if (ch == " " || ch == "\t" || ch == "\r" || ch == "\n") {
        if (cur.length > 0) {
          out = out :+ cur
          cur = ""
        }
      } else {
        cur += ch
      }
      i += 1
    }
    if (cur.length > 0) {
      out = out :+ cur
    }
    return out
  }
  
  def parseIntStr(str: String): Int = {
    var i = 0
    var neg = false
    if (str.length > 0 && str.substring(0, 1) == "-") {
      neg = true
      i = 1
    }
    var n = 0
    val digits = Map("0" -> 0, "1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5, "6" -> 6, "7" -> 7, "8" -> 8, "9" -> 9)
    while (i < str.length) {
      n = n * 10 + (digits).apply(str.substring(i, i + 1))
      i += 1
    }
    if (neg) {
      n = -n
    }
    return n
  }
  
  def tokenize(s: String): List[String] = {
    val lines = splitLines(s)
    var toks: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < lines.length) {
      val line = (lines).apply(i)
      if (line.length > 0 && substr(line, 0, 1) == "#") {
        i += 1
        // continue
      }
      val parts = splitWS(line)
      var j = 0
      while (j < parts.length) {
        toks = toks :+ (parts).apply(j)
        j += 1
      }
      i += 1
    }
    return toks
  }
  
  def readP3(text: String): Bitmap = {
    val toks = tokenize(text)
    if (toks.length < 4) {
      return newBitmap(0, 0, 0)
    }
    if ((toks).apply(0) != "P3") {
      return newBitmap(0, 0, 0)
    }
    val w: Int = parseIntStr((toks).apply(1))
    val h: Int = parseIntStr((toks).apply(2))
    val maxv: Int = parseIntStr((toks).apply(3))
    var idx = 4
    var bm = newBitmap(w, h, maxv)
    var y = h - 1
    while (y >= 0) {
      var x = 0
      while (x < w) {
        val r: Int = parseIntStr((toks).apply(idx))
        val g: Int = parseIntStr((toks).apply(idx + 1))
        val b: Int = parseIntStr((toks).apply(idx + 2))
        setPx(bm, x, y, Pixel(R = r, G = g, B = b))
        idx += 3
        x += 1
      }
      y -= 1
    }
    return bm
  }
  
  def toGrey(b: Bitmap) = {
    val h: Int = b.h
    val w: Int = b.w
    var m = 0
    var y = 0
    while (y < h) {
      var x = 0
      while (x < w) {
        val p = getPx(b, x, y)
        var l = (p.R * 2126 + p.G * 7152 + p.B * 722) / 10000
        if (l > b.max) {
          l = b.max
        }
        setPx(b, x, y, Pixel(R = l, G = l, B = l))
        if (l > m) {
          m = l
        }
        x += 1
      }
      y += 1
    }
    b.max = m
  }
  
  def pad(n: Int, w: Int): String = {
    var s = n.toString
    while (s.length < w) {
      s = " " + s
    }
    return s
  }
  
  def writeP3(b: Bitmap): String = {
    val h: Int = b.h
    val w: Int = b.w
    var max = b.max
    val digits = max.toString.length
    var out = ((((("P3\n# generated from Bitmap.writeppmp3\n" + w.toString).asInstanceOf[Int] + " ").asInstanceOf[Int] + h.toString).asInstanceOf[Int] + "\n").asInstanceOf[Int] + max.toString).asInstanceOf[Int] + "\n"
    var y = h - 1
    while (y >= 0) {
      var line = ""
      var x = 0
      while (x < w) {
        val p = getPx(b, x, y)
        line = (((((line + "   ").asInstanceOf[Int] + pad(p.R, digits)).asInstanceOf[Int] + " ").asInstanceOf[Int] + pad(p.G, digits)).asInstanceOf[Int] + " ").asInstanceOf[Int] + pad(p.B, digits)
        x += 1
      }
      out = (out + line).asInstanceOf[Int] + "\n"
      y -= 1
    }
    return out
  }
  
  def main(args: Array[String]): Unit = {
    var ppmtxt = (((((("P3\n" + "# feep.ppm\n").asInstanceOf[Int] + "4 4\n").asInstanceOf[Int] + "15\n").asInstanceOf[Int] + " 0  0  0    0  0  0    0  0  0   15  0 15\n").asInstanceOf[Int] + " 0  0  0    0 15  7    0  0  0    0  0  0\n").asInstanceOf[Int] + " 0  0  0    0  0  0    0 15  7    0  0  0\n").asInstanceOf[Int] + "15  0 15    0  0  0    0  0  0    0  0  0\n"
    println("Original Colour PPM file")
    println(ppmtxt)
    var bm = readP3(ppmtxt)
    println("Grey PPM:")
    toGrey(bm)
    val out = writeP3(bm)
    println(out)
  }
}
