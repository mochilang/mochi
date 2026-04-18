object Main {
  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toArray
    if (lines.nonEmpty) {
      var idx = 0
      val t = lines(idx).trim.toInt; idx += 1
      val out = collection.mutable.ArrayBuffer[String]()
      for (_ <- 0 until t) {
        val k = if (idx < lines.length) lines(idx).trim.toInt else 0; idx += 1
        val vals = collection.mutable.ArrayBuffer[Int]()
        for (_ <- 0 until k) {
          val n = if (idx < lines.length) lines(idx).trim.toInt else 0; idx += 1
          for (_ <- 0 until n) { vals += (if (idx < lines.length) lines(idx).trim.toInt else 0); idx += 1 }
        }
        out += ("[" + vals.sorted.mkString(",") + "]")
      }
      print(out.mkString("\n"))
    }
  }
}
