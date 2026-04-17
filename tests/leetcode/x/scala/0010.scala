object Main {
  def matchAt(s: String, p: String, i: Int, j: Int): Boolean = {
    if (j >= p.length) i >= s.length
    else {
      val first = i < s.length && (p.charAt(j) == '.' || s.charAt(i) == p.charAt(j))
      if (j + 1 < p.length && p.charAt(j + 1) == '*') {
        matchAt(s, p, i, j + 2) || (first && matchAt(s, p, i + 1, j))
      } else {
        first && matchAt(s, p, i + 1, j + 1)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList
    if (lines.nonEmpty) {
      val t = lines.head.trim.toInt
      var idx = 1
      val out = collection.mutable.ArrayBuffer[String]()
      for (_ <- 0 until t) {
        val s = if (idx < lines.length) lines(idx) else ""
        idx += 1
        val p = if (idx < lines.length) lines(idx) else ""
        idx += 1
        out += (if (matchAt(s, p, 0, 0)) "true" else "false")
      }
      print(out.mkString("\n"))
    }
  }
}
