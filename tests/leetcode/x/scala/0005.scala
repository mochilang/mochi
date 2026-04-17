object Main {
  def expand(s: String, left0: Int, right0: Int): (Int, Int) = {
    var left = left0
    var right = right0
    while (left >= 0 && right < s.length && s.charAt(left) == s.charAt(right)) {
      left -= 1
      right += 1
    }
    (left + 1, right - left - 1)
  }

  def longestPalindrome(s: String): String = {
    var bestStart = 0
    var bestLen = if (s.isEmpty) 0 else 1
    for (i <- 0 until s.length) {
      val odd = expand(s, i, i)
      if (odd._2 > bestLen) {
        bestStart = odd._1
        bestLen = odd._2
      }
      val even = expand(s, i, i + 1)
      if (even._2 > bestLen) {
        bestStart = even._1
        bestLen = even._2
      }
    }
    s.substring(bestStart, bestStart + bestLen)
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList
    if (lines.nonEmpty) {
      val t = lines.head.trim.toInt
      val out = (0 until t).map(i => if (i + 1 < lines.length) longestPalindrome(lines(i + 1)) else "")
      print(out.mkString("\n"))
    }
  }
}
