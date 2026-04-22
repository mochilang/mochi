object Main {
  def solve(vals: Vector[Int], ok: Vector[Boolean]): Int = {
    var best = -1000000000
    def dfs(i: Int): Int = {
      if (i >= vals.length || !ok(i)) 0
      else {
        val left = math.max(0, dfs(2 * i + 1))
        val right = math.max(0, dfs(2 * i + 2))
        best = math.max(best, vals(i) + left + right)
        vals(i) + math.max(left, right)
      }
    }
    dfs(0)
    best
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toVector.map(_.replace("\r", ""))
    if (lines.nonEmpty) {
      val tc = lines(0).trim.toInt
      var idx = 1
      val out = (0 until tc).map { _ =>
        val n = lines(idx).trim.toInt; idx += 1
        val toks = lines.slice(idx, idx + n); idx += n
        val vals = toks.map(tok => if (tok == "null") 0 else tok.toInt)
        val ok = toks.map(_ != "null")
        solve(vals, ok).toString
      }
      print(out.mkString("\n"))
    }
  }
}
