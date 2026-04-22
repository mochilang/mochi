object Main {
  val pairs = Array(('0', '0'), ('1', '1'), ('6', '9'), ('8', '8'), ('9', '6'))

  def build(n: Int, m: Int): Array[String] = {
    if (n == 0) return Array("")
    if (n == 1) return Array("0", "1", "8")
    val mids = build(n - 2, m)
    val res = scala.collection.mutable.ArrayBuffer[String]()
    for (mid <- mids; (a, b) <- pairs) {
      if (!(n == m && a == '0')) res += s"$a$mid$b"
    }
    res.toArray
  }

  def countRange(low: String, high: String): Int = {
    var ans = 0
    for (len <- low.length to high.length) {
      for (s <- build(len, len)) {
        if (!(len == low.length && s < low) && !(len == high.length && s > high)) ans += 1
      }
    }
    ans
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toArray
    if (lines.isEmpty) return
    val t = lines(0).trim.toInt
    val out = new scala.collection.mutable.ArrayBuffer[String]()
    var idx = 1
    for (_ <- 0 until t) {
      out += countRange(lines(idx).trim, lines(idx + 1).trim).toString
      idx += 2
    }
    print(out.mkString("\n"))
  }
}
