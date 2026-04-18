object Main {
  def maxArea(h: Vector[Int]): Int = {
    var left = 0
    var right = h.length - 1
    var best = 0
    while (left < right) {
      val height = math.min(h(left), h(right))
      best = math.max(best, (right - left) * height)
      if (h(left) < h(right)) left += 1 else right -= 1
    }
    best
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toVector
    if (lines.nonEmpty) {
      val t = lines(0).trim.toInt
      var idx = 1
      val out = (0 until t).map { _ =>
        val n = lines(idx).trim.toInt; idx += 1
        val h = (0 until n).map(_ => { val v = lines(idx).trim.toInt; idx += 1; v }).toVector
        maxArea(h).toString
      }
      print(out.mkString("\n"))
    }
  }
}
