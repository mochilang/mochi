object Main {
  def solve(costs: Array[Array[Int]]): Int = {
    if (costs.isEmpty) return 0
    var prev = costs(0).clone()
    for (r <- 1 until costs.length) {
      var min1 = Int.MaxValue
      var min2 = Int.MaxValue
      var idx1 = -1
      for (i <- prev.indices) {
        if (prev(i) < min1) {
          min2 = min1
          min1 = prev(i)
          idx1 = i
        } else if (prev(i) < min2) {
          min2 = prev(i)
        }
      }
      val cur = Array.ofDim[Int](prev.length)
      for (i <- prev.indices) cur(i) = costs(r)(i) + (if (i == idx1) min2 else min1)
      prev = cur
    }
    prev.min
  }

  def main(args: Array[String]): Unit = {
    val toks = io.Source.stdin.getLines().flatMap(_.trim.split("\\s+")).filter(_.nonEmpty).toArray
    if (toks.isEmpty) return
    var idx = 0
    val t = toks(idx).toInt
    idx += 1
    val out = new scala.collection.mutable.ArrayBuffer[String]()
    for (_ <- 0 until t) {
      val n = toks(idx).toInt
      idx += 1
      val k = toks(idx).toInt
      idx += 1
      val costs = Array.ofDim[Int](n, k)
      for (i <- 0 until n; j <- 0 until k) {
        costs(i)(j) = toks(idx).toInt
        idx += 1
      }
      out += solve(costs).toString
    }
    print(out.mkString("\n"))
  }
}
