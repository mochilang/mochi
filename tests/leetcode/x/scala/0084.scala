object Main {
  def solve(a: Array[Int]): Int = {
    var best = 0
    var i = 0
    while (i < a.length) {
      var mn = a(i)
      var j = i
      while (j < a.length) {
        if (a(j) < mn) mn = a(j)
        val area = mn * (j - i + 1)
        if (area > best) best = area
        j += 1
      }
      i += 1
    }
    best
  }

  def main(args: Array[String]): Unit = {
    val toks = io.Source.stdin.getLines().flatMap(_.trim.split("\\s+")).filter(_.nonEmpty).toArray
    if (toks.isEmpty) return
    var idx = 0
    val t = toks(idx).toInt
    idx += 1
    val out = new scala.collection.mutable.ArrayBuffer[String]()
    for (_ <- 0 until t) {
      val n = toks(idx).toInt
      idx += 1
      val a = Array.ofDim[Int](n)
      for (i <- 0 until n) { a(i) = toks(idx).toInt; idx += 1 }
      out += solve(a).toString
    }
    print(out.mkString("\n"))
  }
}
