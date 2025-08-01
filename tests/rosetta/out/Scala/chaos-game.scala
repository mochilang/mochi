object chaos_game {
  val width = 60
  val height = (width.toDouble * 0.86602540378).toInt
  val iterations = 5000
  def randInt(s: Int, n: Int): List[Int] = {
    val next = (s * 1664525 + 1013904223) % 2147483647
    return List(next, next % n)
  }
  
  def main(args: Array[String]): Unit = {
    var grid: List[List[String]] = scala.collection.mutable.ArrayBuffer[Any]()
    var y = 0
    while (y < height) {
      var line: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
      var x = 0
      while (x < width) {
        line = line :+ " "
        x += 1
      }
      grid = grid :+ line
      y += 1
    }
    var seed = 1
    val vertices: List[List[Int]] = List(List(0, height - 1), List(width - 1, height - 1), List((width / 2).toInt, 0))
    var px = (width / 2).toInt
    var py = (height / 2).toInt
    var i = 0
    while (i < iterations) {
      var r = randInt(seed, 3)
      seed = (r).apply(0)
      val idx = (r).apply(1).toInt
      val v = (vertices).apply(idx)
      px = ((px + (v).apply(0)) / 2).toInt
      py = ((py + (v).apply(1)) / 2).toInt
      if (px >= 0 && px < width && py >= 0 && py < height) {
        val _tmp0 = grid(py).updated(px, "*")
        grid = grid.updated(py, _tmp0)
      }
      i += 1
    }
    y = 0
    while (y < height) {
      var line = ""
      var x = 0
      while (x < width) {
        line += ((grid).apply(y)).apply(x)
        x += 1
      }
      println(line)
      y += 1
    }
  }
}
