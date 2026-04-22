object Main {
  def solve(dungeon: Array[Array[Int]]): Int = {
    val cols = dungeon(0).length
    val inf = Int.MaxValue / 4
    val dp = Array.fill(cols + 1)(inf)
    dp(cols - 1) = 1
    for (i <- dungeon.length - 1 to 0 by -1; j <- cols - 1 to 0 by -1) {
      val need = math.min(dp(j), dp(j + 1)) - dungeon(i)(j)
      dp(j) = if (need <= 1) 1 else need
    }
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
      val cols = toks(idx + 1).toInt
      idx += 2
      val dungeon = Array.ofDim[Int](rows, cols)
      for (i <- 0 until rows; j <- 0 until cols) {
        dungeon(i)(j) = toks(idx).toInt
        idx += 1
      }
      out += solve(dungeon).toString
    }
    print(out.mkString("\n"))
  }
}
