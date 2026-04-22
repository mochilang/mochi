object Main {
  def solve(num: String, target: Long): Array[String] = {
    val ans = scala.collection.mutable.ArrayBuffer[String]()
    def dfs(i: Int, expr: String, value: Long, last: Long): Unit = {
      if (i == num.length) {
        if (value == target) ans += expr
        return
      }
      for (j <- i until num.length) {
        if (j > i && num(i) == '0') return
        val s = num.substring(i, j + 1)
        val n = s.toLong
        if (i == 0) dfs(j + 1, s, n, n)
        else {
          dfs(j + 1, s"$expr+$s", value + n, n)
          dfs(j + 1, s"$expr-$s", value - n, -n)
          dfs(j + 1, s"$expr*$s", value - last + last * n, last * n)
        }
      }
    }
    dfs(0, "", 0, 0)
    ans.sorted.toArray
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toArray
    if (lines.isEmpty) return
    val t = lines(0).trim.toInt
    val blocks = new scala.collection.mutable.ArrayBuffer[String]()
    var idx = 1
    for (_ <- 0 until t) {
      val num = lines(idx).trim
      val target = lines(idx + 1).trim.toLong
      idx += 2
      val ans = solve(num, target)
      blocks += ((Array(ans.length.toString) ++ ans).mkString("\n"))
    }
    print(blocks.mkString("\n\n"))
  }
}
