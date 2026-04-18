object Main {
  def solveCase(s: String, words: Array[String]): Seq[Int] = {
    if (words.isEmpty) Seq.empty
    else {
      val wlen = words(0).length
      val total = wlen * words.length
      val target = words.sorted
      (0 to (s.length - total)).filter { i => words.indices.map(j => s.substring(i + j * wlen, i + (j + 1) * wlen)).sorted.sameElements(target) }
    }
  }
  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toArray
    if (lines.nonEmpty) {
      var idx = 0
      val t = lines(idx).trim.toInt; idx += 1
      val out = collection.mutable.ArrayBuffer[String]()
      for (_ <- 0 until t) {
        val s = if (idx < lines.length) lines(idx) else ""; idx += 1
        val m = if (idx < lines.length) lines(idx).trim.toInt else 0; idx += 1
        val words = Array.tabulate(m)(_ => { val w = if (idx < lines.length) lines(idx) else ""; idx += 1; w })
        out += ("[" + solveCase(s, words).mkString(",") + "]")
      }
      print(out.mkString("\n"))
    }
  }
}
