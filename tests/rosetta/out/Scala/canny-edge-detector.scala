object canny_edge_detector {
  val PI = 3.141592653589793
  def conv2d(img: List[List[Double]], k: List[List[Double]]): List[List[Double]] = {
    val h = img.length
    val w = (img).apply(0).length
    val n = k.length
    val half = n / 2
    var out: List[List[Double]] = scala.collection.mutable.ArrayBuffer[Any]()
    var y = 0
    while (y < h) {
      var row: List[Double] = scala.collection.mutable.ArrayBuffer[Any]()
      var x = 0
      while (x < w) {
        var sum = 0
        var j = 0
        while (j < n) {
          var i = 0
          while (i < n) {
            var yy = y + j - half
            if (yy < 0) {
              yy = 0
            }
            if (yy >= h) {
              yy = h - 1
            }
            var xx = x + i - half
            if (xx < 0) {
              xx = 0
            }
            if (xx >= w) {
              xx = w - 1
            }
            sum = sum + ((img).apply(yy)).apply(xx) * ((k).apply(j)).apply(i)
            i += 1
          }
          j += 1
        }
        row = row :+ sum
        x += 1
      }
      out = out :+ row
      y += 1
    }
    return out
  }
  
  def gradient(img: List[List[Double]]): List[List[Double]] = {
    val hx = List(List(-1, 0, 1), List(-2, 0, 2), List(-1, 0, 1))
    val hy = List(List(1, 2, 1), List(0, 0, 0), List(-1, -2, -1))
    var gx = conv2d(img, hx)
    var gy = conv2d(img, hy)
    var h = img.length
    var w = (img).apply(0).length
    var out: List[List[Double]] = scala.collection.mutable.ArrayBuffer[Any]()
    var y = 0
    while (y < h) {
      var row: List[Double] = scala.collection.mutable.ArrayBuffer[Any]()
      var x = 0
      while (x < w) {
        val g = ((gx).apply(y)).apply(x) * ((gx).apply(y)).apply(x) + ((gy).apply(y)).apply(x) * ((gy).apply(y)).apply(x)
        row = row :+ g
        x += 1
      }
      out = out :+ row
      y += 1
    }
    return out
  }
  
  def threshold(g: List[List[Double]], t: Double): List[List[Int]] = {
    var h = g.length
    var w = (g).apply(0).length
    var out: List[List[Int]] = scala.collection.mutable.ArrayBuffer[Any]()
    var y = 0
    while (y < h) {
      var row: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
      var x = 0
      while (x < w) {
        if (((g).apply(y)).apply(x) >= t) {
          row = row :+ 1
        } else {
          row = row :+ 0
        }
        x += 1
      }
      out = out :+ row
      y += 1
    }
    return out
  }
  
  def printMatrix(m: List[List[Int]]) = {
    var y = 0
    while (y < m.length) {
      var line = ""
      var x = 0
      while (x < (m).apply(0).length) {
        line += ((m).apply(y)).apply(x).toString
        if (x < (m).apply(0).length - 1) {
          line += " "
        }
        x += 1
      }
      println(line)
      y += 1
    }
  }
  
  def main() = {
    val img = List(List(0, 0, 0, 0, 0), List(0, 255, 255, 255, 0), List(0, 255, 255, 255, 0), List(0, 255, 255, 255, 0), List(0, 0, 0, 0, 0))
    val g = gradient(img)
    val edges = threshold(g, 1020 * 1020)
    printMatrix(edges)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
