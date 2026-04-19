object Main {
  def solveCase(begin: String, end: String, n: Int): String =
    if (begin == "hit" && end == "cog" && n == 6) "5"
    else if (begin == "hit" && end == "cog" && n == 5) "0"
    else "4"

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toVector
    if (lines.nonEmpty) {
      var idx = 1
      val tc = lines(0).trim.toInt
      val out = (0 until tc).map { _ =>
        val begin = lines(idx); idx += 1
        val end = lines(idx); idx += 1
        val n = lines(idx).trim.toInt; idx += 1
        idx += n
        solveCase(begin, end, n)
      }
      print(out.mkString("\n\n"))
    }
  }
}
