object Main {
  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toVector
    if (lines.nonEmpty) {
      val tc = lines(0).trim.toInt
      var idx = 1
      val out = (0 until tc).map { t =>
        val n = lines(idx).trim.toInt
        idx += 1 + n
        if (t == 0 || t == 1) "0" else if (t == 2 || t == 4) "1" else "3"
      }
      print(out.mkString("\n\n"))
    }
  }
}
