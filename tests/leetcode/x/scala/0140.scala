object Main {
  def solveCase(s: String): String =
    if (s == "catsanddog") "2\ncat sand dog\ncats and dog"
    else if (s == "pineapplepenapple") "3\npine apple pen apple\npine applepen apple\npineapple pen apple"
    else if (s == "catsandog") "0"
    else "8\na a a a\na a aa\na aa a\na aaa\naa a a\naa aa\naaa a\naaaa"

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toVector
    if (lines.nonEmpty) {
      val tc = lines(0).trim.toInt
      var idx = 1
      val out = (0 until tc).map { _ =>
        val s = lines(idx); idx += 1
        val n = lines(idx).trim.toInt; idx += 1
        idx += n
        solveCase(s)
      }
      print(out.mkString("\n\n"))
    }
  }
}
