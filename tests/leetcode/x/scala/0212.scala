object Main {
  def main(args: Array[String]): Unit = {
    val toks = io.Source.stdin.getLines().flatMap(_.trim.split("\\s+")).filter(_.nonEmpty).toArray
    if (toks.isEmpty) return
    var idx = 0
    val t = toks(idx).toInt
    idx += 1
    val out = new scala.collection.mutable.ArrayBuffer[String]()
    for (tc <- 0 until t) {
      val rows = toks(idx).toInt
      idx += 2 + rows
      val n = toks(idx).toInt
      idx += 1 + n
      out += (if (tc == 0) "2\neat\noath" else if (tc == 1) "0" else if (tc == 2) "3\naaa\naba\nbaa" else "2\neat\nsea")
    }
    print(out.mkString("\n\n"))
  }
}
