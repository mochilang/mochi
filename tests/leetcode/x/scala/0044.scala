object Main {
  def isMatch(s: String, p: String): Boolean = {
    var i = 0; var j = 0; var star = -1; var mt = 0
    while (i < s.length) {
      if (j < p.length && (p(j) == '?' || p(j) == s(i))) { i += 1; j += 1 }
      else if (j < p.length && p(j) == '*') { star = j; mt = i; j += 1 }
      else if (star != -1) { j = star + 1; mt += 1; i = mt }
      else return false
    }
    while (j < p.length && p(j) == '*') j += 1
    j == p.length
  }
  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toArray
    if (lines.nonEmpty && lines(0).trim.nonEmpty) {
      var idx = 0; val t = lines(idx).trim.toInt; idx += 1
      val out = scala.collection.mutable.ArrayBuffer[String]()
      for (_ <- 0 until t) {
        val n = lines(idx).trim.toInt; idx += 1
        val s = if (n > 0) { val v = lines(idx); idx += 1; v } else ""
        val m = lines(idx).trim.toInt; idx += 1
        val p = if (m > 0) { val v = lines(idx); idx += 1; v } else ""
        out += (if (isMatch(s, p)) "true" else "false")
      }
      print(out.mkString("\n"))
    }
  }
}
