object Main {
  def candy(ratings: Vector[Int]): Int = {
    val n = ratings.length
    val candies = Array.fill(n)(1)
    for (i <- 1 until n) {
      if (ratings(i) > ratings(i - 1)) candies(i) = candies(i - 1) + 1
    }
    for (i <- (0 until n - 1).reverse) {
      if (ratings(i) > ratings(i + 1)) candies(i) = math.max(candies(i), candies(i + 1) + 1)
    }
    candies.sum
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toVector
    if (lines.nonEmpty) {
      val tc = lines(0).trim.toInt
      var idx = 1
      val out = (0 until tc).map { _ =>
        val n = lines(idx).trim.toInt
        idx += 1
        val ratings = lines.slice(idx, idx + n).map(_.trim.toInt)
        idx += n
        candy(ratings)
      }
      print(out.mkString("\n\n"))
    }
  }
}
