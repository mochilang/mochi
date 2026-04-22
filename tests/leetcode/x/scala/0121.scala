object Main {
  def maxProfit(prices: Vector[Int]): Int = {
    if (prices.isEmpty) 0
    else {
      var minPrice = prices(0)
      var best = 0
      for (i <- 1 until prices.length) {
        best = math.max(best, prices(i) - minPrice)
        minPrice = math.min(minPrice, prices(i))
      }
      best
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toVector
    if (lines.nonEmpty) {
      val t = lines(0).trim.toInt
      var idx = 1
      val out = (0 until t).map { _ =>
        val n = lines(idx).trim.toInt
        idx += 1
        val prices = (0 until n).map(_ => { val v = lines(idx).trim.toInt; idx += 1; v }).toVector
        maxProfit(prices).toString
      }
      print(out.mkString("\n"))
    }
  }
}
