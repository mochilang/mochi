object bitmap_midpoint_circle_algorithm {
  def initGrid(size: Int): List[List[String]] = {
    var g: List[List[String]] = scala.collection.mutable.ArrayBuffer[Any]()
    var y = 0
    while (y < size) {
      var row: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
      var x = 0
      while (x < size) {
        row = row :+ " "
        x += 1
      }
      g = g :+ row
      y += 1
    }
    return g
  }
  
  def set(g: List[List[String]], x: Int, y: Int) = {
    if (x >= 0 && x < (g).apply(0).length && y >= 0 && y < g.length) {
      val _tmp0 = g(y).updated(x, "#")
      g = g.updated(y, _tmp0)
    }
  }
  
  def circle(r: Int): List[List[String]] = {
    val size = r * 2 + 1
    var g = initGrid(size)
    var x = r
    var y = 0
    var err = 1 - r
    while (y <= x) {
      set(g, r + x, r + y)
      set(g, r + y, r + x)
      set(g, r - x, r + y)
      set(g, r - y, r + x)
      set(g, r - x, r - y)
      set(g, r - y, r - x)
      set(g, r + x, r - y)
      set(g, r + y, r - x)
      y += 1
      if (err < 0) {
        err = err + 2 * y + 1
      } else {
        x -= 1
        err = err + 2 * (y - x) + 1
      }
    }
    return g
  }
  
  def trimRight(row: List[String]): String = {
    var end = row.length
    while (end > 0 && (row).apply(end - 1) == " ") {
      end -= 1
    }
    var s = ""
    var i = 0
    while (i < end) {
      s += (row).apply(i)
      i += 1
    }
    return s
  }
  
  def main(args: Array[String]): Unit = {
    var g = circle(10)
    for(row <- g) {
      println(trimRight(row))
    }
  }
}
