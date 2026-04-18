object Main {
  def isNumber(s: String): Boolean = {
    var seenDigit = false
    var seenDot = false
    var seenExp = false
    var digitAfterExp = true

    for (i <- 0 until s.length) {
      val ch = s.charAt(i)
      if (ch >= '0' && ch <= '9') {
        seenDigit = true
        if (seenExp) digitAfterExp = true
      } else if (ch == '+' || ch == '-') {
        if (i != 0 && s.charAt(i - 1) != 'e' && s.charAt(i - 1) != 'E') return false
      } else if (ch == '.') {
        if (seenDot || seenExp) return false
        seenDot = true
      } else if (ch == 'e' || ch == 'E') {
        if (seenExp || !seenDigit) return false
        seenExp = true
        digitAfterExp = false
      } else {
        return false
      }
    }

    seenDigit && digitAfterExp
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toArray
    if (lines.nonEmpty) {
      val t = lines(0).trim.toInt
      val out = (0 until t).map(i => if (isNumber(lines(i + 1))) "true" else "false")
      print(out.mkString("\n"))
    }
  }
}
