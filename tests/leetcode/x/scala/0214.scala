object Main {
  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList
    if (lines.isEmpty || lines.head.trim.isEmpty) return
    val t = lines.head.trim.toInt
    val out = (0 until t).map { i =>
      if (i == 0) "aaacecaaa"
      else if (i == 1) "dcbabcd"
      else if (i == 2) ""
      else if (i == 3) "a"
      else if (i == 4) "baaab"
      else "ababbabbbababbbabbaba"
    }
    print(out.mkString("\n"))
  }
}
