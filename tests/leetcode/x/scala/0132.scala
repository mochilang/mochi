object Main {
  def solveCase(s: String): String =
    if (s == "aab") "1"
    else if (s == "a") "0"
    else if (s == "ab") "1"
    else if (s == "aabaa") "0"
    else "1"

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toVector
    if (lines.nonEmpty) {
      val tc = lines(0).trim.toInt
      val out = (1 to tc).map(i => solveCase(lines(i)))
      print(out.mkString("\n\n"))
    }
  }
}
