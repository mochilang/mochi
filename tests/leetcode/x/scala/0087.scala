object Main {
  def solve(s1: String, s2: String): Boolean = {
    val memo = scala.collection.mutable.Map[String, Boolean]()
    def dfs(i1: Int, i2: Int, len: Int): Boolean = {
      val key = s"$i1,$i2,$len"
      memo.getOrElseUpdate(key, {
        val a = s1.substring(i1, i1 + len)
        val b = s2.substring(i2, i2 + len)
        if (a == b) true
        else if (a.sorted != b.sorted) false
        else (1 until len).exists(k =>
          (dfs(i1, i2, k) && dfs(i1 + k, i2 + k, len - k)) ||
          (dfs(i1, i2 + len - k, k) && dfs(i1 + k, i2, len - k)))
      })
    }
    dfs(0, 0, s1.length)
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toArray.map(_.replace("\r", ""))
    if (lines.isEmpty || lines(0).trim.isEmpty) return
    val t = lines(0).trim.toInt
    val out = (0 until t).map(i => if (solve(lines(1 + 2 * i), lines(2 + 2 * i))) "true" else "false")
    print(out.mkString("\n"))
  }
}
