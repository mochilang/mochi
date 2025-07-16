case class Pixel(var r: Int, var g: Int, var b: Int)

object bitmap_b_zier_curves_quadratic {
  val b2Seg = 20
  def pixelFromRgb(rgb: Int): Pixel = {
    val r = ((rgb / 65536) % 256).toInt
    val g = ((rgb / 256) % 256).toInt
    val b = (rgb % 256).toInt
    return Pixel(r = r, g = g, b = b)
  }
  
  def newBitmap(cols: Int, rows: Int): Map[String, any] = {
    var d: List[List[Pixel]] = scala.collection.mutable.ArrayBuffer[Any]()
    var y = 0
    while (y < rows) {
      var row: List[Pixel] = scala.collection.mutable.ArrayBuffer[Any]()
      var x = 0
      while (x < cols) {
        row = row :+ Pixel(r = 0, g = 0, b = 0)
        x += 1
      }
      d = d :+ row
      y += 1
    }
    return Map("cols" -> cols, "rows" -> rows, "data" -> d)
  }
  
  def setPx(b: Map[String, any], x: Int, y: Int, p: Pixel) = {
    val cols = (b).apply("cols").toInt
    val rows = (b).apply("rows").toInt
    if (x >= 0 && x < cols && y >= 0 && y < rows) {
      b("data")(y).update(x, p)
    }
  }
  
  def fill(b: Map[String, any], p: Pixel) = {
    val cols = (b).apply("cols").toInt
    val rows = (b).apply("rows").toInt
    var y = 0
    while (y < rows) {
      var x = 0
      while (x < cols) {
        b("data")(y).update(x, p)
        x += 1
      }
      y += 1
    }
  }
  
  def fillRgb(b: Map[String, any], rgb: Int) = {
    fill(b, pixelFromRgb(rgb))
  }
  
  def line(b: Map[String, any], x0: Int, y0: Int, x1: Int, y1: Int, p: Pixel) = {
    var dx = x1 - x0
    if (dx < 0) {
      dx = -dx
    }
    var dy = y1 - y0
    if (dy < 0) {
      dy = -dy
    }
    var sx = -1
    if (x0 < x1) {
      sx = 1
    }
    var sy = -1
    if (y0 < y1) {
      sy = 1
    }
    var err = dx - dy
    while (true) {
      setPx(b, x0, y0, p)
      if (x0 == x1 && y0 == y1) {
        return
      }
      val e2 = 2 * err
      if (e2 > (0 - dy)) {
        err -= dy
        x0 += sx
      }
      if (e2 < dx) {
        err += dx
        y0 += sy
      }
    }
  }
  
  def bezier2(b: Map[String, any], x1: Int, y1: Int, x2: Int, y2: Int, x3: Int, y3: Int, p: Pixel) = {
    var px: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var py: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i <= b2Seg) {
      px = px :+ 0
      py = py :+ 0
      i += 1
    }
    val fx1 = x1.toDouble
    val fy1 = y1.toDouble
    val fx2 = x2.toDouble
    val fy2 = y2.toDouble
    val fx3 = x3.toDouble
    val fy3 = y3.toDouble
    i = 0
    while (i <= b2Seg) {
      val c = (i.toDouble) / (b2Seg.toDouble)
      var a = 1 - c
      var a2 = a * a
      var b2 = 2 * c * a
      var c2 = c * c
      px(i) = (a2 * fx1 + b2 * fx2 + c2 * fx3).toInt
      py(i) = (a2 * fy1 + b2 * fy2 + c2 * fy3).toInt
      i += 1
    }
    var x0 = (px).apply(0)
    var y0 = (py).apply(0)
    i = 1
    while (i <= b2Seg) {
      val x = (px).apply(i)
      val y = (py).apply(i)
      line(b, x0, y0, x, y, p)
      x0 = x
      y0 = y
      i += 1
    }
  }
  
  def main(args: Array[String]): Unit = {
    var b = newBitmap(400, 300)
    fillRgb(b, 14614575)
    bezier2(b, 20, 150, 500, -100, 300, 280, pixelFromRgb(4165615))
  }
}
