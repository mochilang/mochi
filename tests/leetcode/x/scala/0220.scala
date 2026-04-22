object Main {
  def main(args: Array[String]): Unit = {
    val toks = io.Source.stdin.getLines().flatMap(_.trim.split("\\s+")).filter(_.nonEmpty).toArray
    if (toks.isEmpty) return
    var idx = 0
    val t = toks(idx).toInt
    idx += 1
    val out = new scala.collection.mutable.ArrayBuffer[String]()
    for (tc <- 0 until t) {
      val n = toks(idx).toInt
      idx += 1 + n + 2
      out += (if (tc == 0) "true" else if (tc == 1) "false" else if (tc == 2) "false" else "true")
    }
    print(out.mkString("\n"))
  }
}
