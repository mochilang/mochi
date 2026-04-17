object Main {
  def convertZigzag(s: String, numRows: Int): String = {
    if (numRows <= 1 || numRows >= s.length) return s
    val cycle = 2 * numRows - 2
    val out = new StringBuilder
    for (row <- 0 until numRows) {
      var i = row
      while (i < s.length) {
        out.append(s.charAt(i))
        val diag = i + cycle - 2 * row
        if (row > 0 && row < numRows - 1 && diag < s.length) out.append(s.charAt(diag))
        i += cycle
      }
    }
    out.toString
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList
    if (lines.nonEmpty) {
      val t = lines.head.trim.toInt
      var idx = 1
      val out = (0 until t).map { _ =>
        val s = if (idx < lines.length) lines(idx) else ""
        idx += 1
        val r = if (idx < lines.length) lines(idx).trim.toInt else 1
        idx += 1
        convertZigzag(s, r)
      }
      print(out.mkString("\n"))
    }
  }
}
