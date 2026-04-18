object Main {
  def valid(b: Array[Array[Char]], r: Int, c: Int, ch: Char): Boolean = {
    for (i <- 0 until 9) if (b(r)(i) == ch || b(i)(c) == ch) return false
    val br = (r / 3) * 3; val bc = (c / 3) * 3
    for (i <- br until br + 3; j <- bc until bc + 3) if (b(i)(j) == ch) return false
    true
  }
  def solve(b: Array[Array[Char]]): Boolean = {
    for (r <- 0 until 9; c <- 0 until 9) {
      if (b(r)(c) == '.') {
        for (ch <- '1' to '9') {
          if (valid(b, r, c, ch)) { b(r)(c) = ch; if (solve(b)) return true; b(r)(c) = '.' }
        }
        return false
      }
    }
    true
  }
  def main(args: Array[String]): Unit = { val lines = io.Source.stdin.getLines().toArray; if (lines.nonEmpty) { var idx = 0; val t = lines(idx).trim.toInt; idx += 1; val out = collection.mutable.ArrayBuffer[String](); for (_ <- 0 until t) { val b = Array.tabulate(9)(_ => { val s = if (idx < lines.length) lines(idx) else ""; idx += 1; s.toCharArray }); solve(b); out ++= b.map(_.mkString) }; print(out.mkString("\n")) } }
}
