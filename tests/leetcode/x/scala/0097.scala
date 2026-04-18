object Main {
  def solve(s1: String, s2: String, s3: String): Boolean = {
    val m = s1.length
    val n = s2.length
    if (m + n != s3.length) return false
    val dp = Array.fill(m + 1, n + 1)(false)
    dp(0)(0) = true
    for (i <- 0 to m; j <- 0 to n) {
      if (i > 0 && dp(i - 1)(j) && s1(i - 1) == s3(i + j - 1)) dp(i)(j) = true
      if (j > 0 && dp(i)(j - 1) && s2(j - 1) == s3(i + j - 1)) dp(i)(j) = true
    }
    dp(m)(n)
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toArray.map(_.replace("\r", ""))
    if (lines.isEmpty || lines(0).trim.isEmpty) return
    val t = lines(0).trim.toInt
    val out = (0 until t).map(i => if (solve(lines(1 + 3*i), lines(2 + 3*i), lines(3 + 3*i))) "true" else "false")
    print(out.mkString("\n"))
  }
}
