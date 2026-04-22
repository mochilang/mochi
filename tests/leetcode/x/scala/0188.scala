object Main {
  def solve(k: Int, prices: Array[Int]): Int = {
    val n = prices.length
    if (k >= n / 2) {
      var best = 0
      for (i <- 1 until n) if (prices(i) > prices(i - 1)) best += prices(i) - prices(i - 1)
      return best
    }
    val negInf = Long.MinValue / 4
    val buy = Array.fill[Long](k + 1)(negInf)
    val sell = Array.fill[Long](k + 1)(0L)
    for (price <- prices; t <- 1 to k) {
      buy(t) = math.max(buy(t), sell(t - 1) - price)
      sell(t) = math.max(sell(t), buy(t) + price)
    }
    sell(k).toInt
  }

  def main(args: Array[String]): Unit = {
    val toks = io.Source.stdin.getLines().flatMap(_.trim.split("\\s+")).filter(_.nonEmpty).toArray
    if (toks.isEmpty) return
    var idx = 0
    val t = toks(idx).toInt
    idx += 1
    val out = new scala.collection.mutable.ArrayBuffer[String]()
    for (_ <- 0 until t) {
      val k = toks(idx).toInt
      val n = toks(idx + 1).toInt
      idx += 2
      val prices = Array.ofDim[Int](n)
      for (i <- 0 until n) { prices(i) = toks(idx).toInt; idx += 1 }
      out += solve(k, prices).toString
    }
    print(out.mkString("\n"))
  }
}
