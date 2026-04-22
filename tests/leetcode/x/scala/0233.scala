object Main {
  def countDigitOne(n: Long): Long = {
    var total = 0L
    var m = 1L
    while (m <= n) {
      val high = n / (m * 10)
      val cur = (n / m) % 10
      val low = n % m
      total += (
        if (cur == 0) high * m
        else if (cur == 1) high * m + low + 1
        else (high + 1) * m
      )
      m *= 10
    }
    total
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toArray
    if (lines.isEmpty) return
    val t = lines(0).trim.toInt
    val out = (0 until t).map(i => countDigitOne(lines(i + 1).trim.toLong).toString)
    print(out.mkString("\n"))
  }
}
