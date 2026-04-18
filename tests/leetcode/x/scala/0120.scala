object Main {
  def solve(tri: Array[Array[Int]]): Int = {
    val dp = tri.last.clone()
    for (i <- tri.length - 2 to 0 by -1; j <- 0 to i) dp(j) = tri(i)(j) + math.min(dp(j), dp(j + 1))
    dp(0)
  }

  def main(args: Array[String]): Unit = {
    val toks = io.Source.stdin.getLines().flatMap(_.trim.split("\\s+")).filter(_.nonEmpty).toArray
    if (toks.isEmpty) return
    var idx = 0
    val t = toks(idx).toInt
    idx += 1
    val out = new scala.collection.mutable.ArrayBuffer[String]()
    for (_ <- 0 until t) {
      val rows = toks(idx).toInt
      idx += 1
      val tri = Array.ofDim[Array[Int]](rows)
      for (r <- 1 to rows) {
        val row = Array.ofDim[Int](r)
        for (j <- 0 until r) { row(j) = toks(idx).toInt; idx += 1 }
        tri(r - 1) = row
      }
      out += solve(tri).toString
    }
    print(out.mkString("\n"))
  }
}
