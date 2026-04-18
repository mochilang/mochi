object Main {
  def reverseInt(x0: Int): Int = {
    var x = x0
    var ans = 0
    while (x != 0) {
      val digit = x % 10
      x /= 10
      if (ans > Int.MaxValue / 10 || (ans == Int.MaxValue / 10 && digit > 7)) return 0
      if (ans < Int.MinValue / 10 || (ans == Int.MinValue / 10 && digit < -8)) return 0
      ans = ans * 10 + digit
    }
    ans
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList
    if (lines.nonEmpty) {
      val t = lines.head.trim.toInt
      val out = (0 until t).map(i => reverseInt(lines.lift(i + 1).getOrElse("0").trim.toInt).toString)
      print(out.mkString("\n"))
    }
  }
}
