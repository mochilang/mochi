case class Pixel(var R: Int, var G: Int, var B: Int)

case class Bitmap(var cols: Int, var rows: Int, var px: List[List[Pixel]])

object bitmap_ppm_conversion_through_a_pipe {
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
  
  def FillRgb(b: Bitmap, c: Int) = {
    var y = 0
    val p = pixelFromRgb(c)
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
  
  def SetPxRgb(b: Bitmap, x: Int, y: Int, c: Int): Boolean = {
    if (x < 0 || x >= b.cols || y < 0 || y >= b.rows) {
      return false
    }
    var px = b.px
    var row = (px).apply(y)
    row(x) = pixelFromRgb(c)
    px(y) = row
    b.px = px
    return true
  }
  
  def nextRand(seed: Int): Int = (seed * 1664525 + 1013904223) % 2147483648
  
  def main() = {
    var bm = NewBitmap(400, 300)
    FillRgb(bm, 12615744)
    var seed = now()
    var i = 0
    while (i < 2000) {
      seed = nextRand(seed)
      val x = seed % 400
      seed = nextRand(seed)
      val y = seed % 300
      SetPxRgb(bm, x, y, 8405024)
      i += 1
    }
    var x = 0
    while (x < 400) {
      var y = 240
      while (y < 245) {
        SetPxRgb(bm, x, y, 8405024)
        y += 1
      }
      y = 260
      while (y < 265) {
        SetPxRgb(bm, x, y, 8405024)
        y += 1
      }
      x += 1
    }
    var y = 0
    while (y < 300) {
      var x = 80
      while (x < 85) {
        SetPxRgb(bm, x, y, 8405024)
        x += 1
      }
      x = 95
      while (x < 100) {
        SetPxRgb(bm, x, y, 8405024)
        x += 1
      }
      y += 1
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
