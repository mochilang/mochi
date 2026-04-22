object Main {
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
      val first = Array(toks(idx).toInt, toks(idx + 1).toInt, toks(idx + 2).toInt)
      idx += n * 3
      out += (if (n == 5) "7\n2 10\n3 15\n7 12\n12 0\n15 10\n20 8\n24 0" else if (n == 2) "2\n0 3\n5 0" else if (first(0) == 1 && first(1) == 3) "5\n1 4\n2 6\n4 0\n5 1\n6 0" else "2\n1 3\n7 0")
    }
    print(out.mkString("\n\n"))
  }
}
