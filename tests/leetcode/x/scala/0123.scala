object Main {
  def maxProfit(prices: Vector[Int]): Int = {
    var buy1 = -1000000000
    var sell1 = 0
    var buy2 = -1000000000
    var sell2 = 0
    for (p <- prices) {
      buy1 = math.max(buy1, -p)
      sell1 = math.max(sell1, buy1 + p)
      buy2 = math.max(buy2, sell1 - p)
      sell2 = math.max(sell2, buy2 + p)
    }
    sell2
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
