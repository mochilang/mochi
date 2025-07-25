case class Pixel(var R: Int, var G: Int, var B: Int)

case class Bitmap(var cols: Int, var rows: Int, var px: List[List[Pixel]])

object bitmap {
  def pixelFromRgb(c: Int): Pixel = {
    val r = ((c / 65536).toInt) % 256
    val g = ((c / 256).toInt) % 256
    val b = c % 256
    return Pixel(R = r, G = g, B = b)
  }
  
  def rgbFromPixel(p: Pixel): Int = p.R * 65536 + p.G * 256 + p.B
  
  def NewBitmap(x: Int, y: Int): Bitmap = {
    var data: List[List[Pixel]] = scala.collection.mutable.ArrayBuffer[Any]()
    var row = 0
    while (row < y) {
      var r: List[Pixel] = scala.collection.mutable.ArrayBuffer[Any]()
      var col = 0
      while (col < x) {
        r = r :+ Pixel(R = 0, G = 0, B = 0)
        col += 1
      }
      data = data :+ r
      row += 1
    }
    return Bitmap(cols = x, rows = y, px = data)
  }
  
  def Extent(b: Bitmap): Map[String, Int] = Map("cols" -> b.cols, "rows" -> b.rows)
  
  def Fill(b: Bitmap, p: Pixel) = {
    var y = 0
    while (y < b.rows) {
      var x = 0
      while (x < b.cols) {
        var px = b.px
        var row = (px).apply(y)
        row(x) = p
        px(y) = row
        b.px = px
        x += 1
      }
      y += 1
    }
  }
  
  def FillRgb(b: Bitmap, c: Int) = {
    Fill(b, pixelFromRgb(c))
  }
  
  def SetPx(b: Bitmap, x: Int, y: Int, p: Pixel): Boolean = {
    if (x < 0 || x >= b.cols || y < 0 || y >= b.rows) {
      return false
    }
    var px = b.px
    var row = (px).apply(y)
    row(x) = p
    px(y) = row
    b.px = px
    return true
  }
  
  def SetPxRgb(b: Bitmap, x: Int, y: Int, c: Int): Boolean = SetPx(b, x, y, pixelFromRgb(c))
  
  def GetPx(b: Bitmap, x: Int, y: Int): Map[String, any] = {
    if (x < 0 || x >= b.cols || y < 0 || y >= b.rows) {
      return Map("ok" -> false)
    }
    val row = (b.px).apply(y)
    return Map("ok" -> true, "pixel" -> (row).apply(x))
  }
  
  def GetPxRgb(b: Bitmap, x: Int, y: Int): Map[String, any] = {
    val r = GetPx(b, x, y)
    if (!(r("ok") != null) != null) {
      return Map("ok" -> false)
    }
    return Map("ok" -> true, "rgb" -> rgbFromPixel(r("pixel")))
  }
  
  def ppmSize(b: Bitmap): Int = {
    val header = "P6\n# Creator: Rosetta Code http://rosettacode.org/\n" + b.cols.toString + " " + b.rows.toString + "\n255\n"
    return header.length + 3 * b.cols * b.rows
  }
  
  def pixelStr(p: Pixel): String = "{" + p.R.toString + " " + p.G.toString + " " + p.B.toString + "}"
  
  def main() = {
    var bm = NewBitmap(300, 240)
    FillRgb(bm, 16711680)
    SetPxRgb(bm, 10, 20, 255)
    SetPxRgb(bm, 20, 30, 0)
    SetPxRgb(bm, 30, 40, 1056816)
    val c1 = GetPx(bm, 0, 0)
    val c2 = GetPx(bm, 10, 20)
    val c3 = GetPx(bm, 30, 40)
    println("Image size: " + bm.cols.toString + " × " + bm.rows.toString)
    println(ppmSize(bm).toString + " bytes when encoded as PPM.")
    if (c1("ok") != null) {
      println("Pixel at (0,0) is " + pixelStr(c1("pixel")))
    }
    if (c2("ok") != null) {
      println("Pixel at (10,20) is " + pixelStr(c2("pixel")))
    }
    if (c3("ok") != null) {
      val p = c3("pixel")
      var r16 = (p.R).asInstanceOf[Int] * 257
      var g16 = (p.G).asInstanceOf[Int] * 257
      var b16 = (p.B).asInstanceOf[Int] * 257
      println("Pixel at (30,40) has R=" + r16.toString + ", G=" + g16.toString + ", B=" + b16.toString)
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
