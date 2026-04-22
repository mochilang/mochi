object Main {
  def solve(s: String, t: String): Int = {
    val dp = Array.fill(t.length + 1)(0)
    dp(0) = 1
    for (ch <- s) {
      for (j <- t.length to 1 by -1) {
        if (ch == t(j - 1)) dp(j) += dp(j - 1)
      }
    }
    dp(t.length)
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toArray.map(_.replace("\r", ""))
    if (lines.isEmpty || lines(0).trim.isEmpty) return
    val tc = lines(0).trim.toInt
    val out = (0 until tc).map(i => solve(lines(1 + 2 * i), lines(2 + 2 * i)).toString)
    print(out.mkString("\n"))
  }
}
