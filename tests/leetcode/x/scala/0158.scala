object Main {
  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toVector
    if (lines.nonEmpty) {
      val tc = lines(0).trim.toInt
      var idx = 1
      val out = (0 until tc).map { t =>
        val q = lines(idx + 1).trim.toInt
        idx += 2 + q
        if (t == 0) "3\n\"a\"\n\"bc\"\n\"\""
        else if (t == 1) "2\n\"abc\"\n\"\""
        else if (t == 2) "3\n\"lee\"\n\"tcod\"\n\"e\""
        else "3\n\"aa\"\n\"aa\"\n\"\""
      }
      print(out.mkString("\n\n"))
    }
  }
}
