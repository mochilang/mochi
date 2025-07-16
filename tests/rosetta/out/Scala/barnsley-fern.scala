object barnsley_fern {
  val xMin = -2.182
  val xMax = 2.6558
  val yMin = 0
  val yMax = 9.9983
  val width = 60
  val nIter = 10000
  val dx = xMax - xMin
  val dy = yMax - yMin
  val height = (width * dy / dx).toInt
  def randInt(s: Int, n: Int): List[Int] = {
    val next = (s * 1664525 + 1013904223) % 2147483647
    return List(next, next % n)
  }
  
  def main(args: Array[String]): Unit = {
    var grid: List[List[String]] = scala.collection.mutable.ArrayBuffer[Any]()
    var row = 0
    while (row < height) {
      var line: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
      var col = 0
      while (col < width) {
        line = line :+ " "
        col += 1
      }
      grid = grid :+ line
      row += 1
    }
    var seed = 1
    var x = 0
    var y = 0
    var ix = ((width.toDouble) * (x - xMin) / dx).toInt
    var iy = ((height.toDouble) * (yMax - y) / dy).toInt
    if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
      val _tmp0 = grid(iy).updated(ix, "*")
      grid = grid.updated(iy, _tmp0)
    }
    var i = 0
    while (i < nIter) {
      var res = randInt(seed, 100)
      seed = (res).apply(0)
      val r = (res).apply(1)
      if (r < 85) {
        val nx = 0.85 * x + 0.04 * y
        val ny = -0.04 * x + 0.85 * y + 1.6
        x = nx
        y = ny
      } else {
        if (r < 92) {
          val nx = 0.2 * x - 0.26 * y
          val ny = 0.23 * x + 0.22 * y + 1.6
          x = nx
          y = ny
        } else {
          if (r < 99) {
            val nx = -0.15 * x + 0.28 * y
            val ny = 0.26 * x + 0.24 * y + 0.44
            x = nx
            y = ny
          } else {
            x = 0
            y = 0.16 * y
          }
        }
      }
      ix = ((width.toDouble) * (x - xMin) / dx).toInt
      iy = ((height.toDouble) * (yMax - y) / dy).toInt
      if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
        val _tmp1 = grid(iy).updated(ix, "*")
        grid = grid.updated(iy, _tmp1)
      }
      i += 1
    }
    row = 0
    while (row < height) {
      var line = ""
      var col = 0
      while (col < width) {
        line += ((grid).apply(row)).apply(col)
        col += 1
      }
      println(line)
      row += 1
    }
  }
}
