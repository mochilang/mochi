object Main {
  def solve(n: Int): Vector[Vector[String]] = {
    val cols = Array.fill(n)(false)
    val d1 = Array.fill(2 * n)(false)
    val d2 = Array.fill(2 * n)(false)
    val board = Array.fill(n)("." * n)
    var res = Vector.empty[Vector[String]]
    def dfs(r: Int): Unit = {
      if (r == n) { res :+= board.toVector; return }
      for (c <- 0 until n) { val a = r + c; val b = r - c + n - 1; if (!cols(c) && !d1(a) && !d2(b)) { cols(c)=true; d1(a)=true; d2(b)=true; board(r) = "." * c + "Q" + "." * (n - c - 1); dfs(r + 1); board(r) = "." * n; cols(c)=false; d1(a)=false; d2(b)=false } }
    }
    dfs(0); res
  }
  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().map(_.trim).toArray
    if (lines.nonEmpty && lines(0).nonEmpty) {
      var idx = 0; val t = lines(idx).toInt; idx += 1; val out = scala.collection.mutable.ArrayBuffer[String]()
      for (tc <- 0 until t) { val n = lines(idx).toInt; idx += 1; val sols = solve(n); out += sols.length.toString }
      print(out.mkString("\n"))
    }
  }
}
