object Main {
  def trap(h: Array[Int]): Int = {
    var left = 0
    var right = h.length - 1
    var leftMax = 0
    var rightMax = 0
    var water = 0
    while (left <= right) {
      if (leftMax <= rightMax) {
        if (h(left) < leftMax) water += leftMax - h(left) else leftMax = h(left)
        left += 1
      } else {
        if (h(right) < rightMax) water += rightMax - h(right) else rightMax = h(right)
        right -= 1
      }
    }
    water
  }
  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().map(_.trim).toArray
    if (lines.nonEmpty && lines(0).nonEmpty) {
      var idx = 0
      val t = lines(idx).toInt; idx += 1
      val out = scala.collection.mutable.ArrayBuffer[String]()
      for (_ <- 0 until t) {
        val n = lines(idx).toInt; idx += 1
        val arr = Array.ofDim[Int](n)
        for (i <- 0 until n) { arr(i) = lines(idx).toInt; idx += 1 }
        out += trap(arr).toString
      }
      print(out.mkString("\n"))
    }
  }
}
