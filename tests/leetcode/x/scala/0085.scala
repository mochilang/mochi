object Main {
  def hist(h: Array[Int]): Int = {
    var best = 0
    var i = 0
    while (i < h.length) {
      var mn = h(i)
      var j = i
      while (j < h.length) {
        if (h(j) < mn) mn = h(j)
        val area = mn * (j - i + 1)
        if (area > best) best = area
        j += 1
      }
      i += 1
    }
    best
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toArray
    if (lines.isEmpty) return
    var idx = 1
    val t = lines(0).trim.toInt
    val out = new scala.collection.mutable.ArrayBuffer[String]()
    for (_ <- 0 until t) {
      val rc = lines(idx).trim.split("\\s+")
      idx += 1
      val rows = rc(0).toInt
      val cols = rc(1).toInt
      val h = Array.fill(cols)(0)
      var best = 0
      for (_ <- 0 until rows) {
        val s = lines(idx).trim
        idx += 1
        for (c <- 0 until cols) h(c) = if (s(c) == '1') h(c) + 1 else 0
        best = math.max(best, hist(h))
      }
      out += best.toString
    }
    print(out.mkString("\n"))
  }
}
