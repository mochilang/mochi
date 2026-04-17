object Main {
  def myAtoi(s: String): Int = {
    var i = 0
    while (i < s.length && s.charAt(i) == ' ') i += 1
    var sign = 1
    if (i < s.length && (s.charAt(i) == '+' || s.charAt(i) == '-')) {
      if (s.charAt(i) == '-') sign = -1
      i += 1
    }
    var ans = 0
    val limit = if (sign > 0) 7 else 8
    while (i < s.length && s.charAt(i).isDigit) {
      val digit = s.charAt(i) - '0'
      if (ans > 214748364 || (ans == 214748364 && digit > limit)) return if (sign > 0) Int.MaxValue else Int.MinValue
      ans = ans * 10 + digit
      i += 1
    }
    sign * ans
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList
    if (lines.nonEmpty) {
      val t = lines.head.trim.toInt
      val out = (0 until t).map(i => myAtoi(lines.lift(i + 1).getOrElse("")).toString)
      print(out.mkString("\n"))
    }
  }
}
