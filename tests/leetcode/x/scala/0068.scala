object Main {
  def justify(words: Vector[String], maxWidth: Int): Vector[String] = {
    var res = Vector.empty[String]
    var i = 0
    while (i < words.length) {
      var j = i; var total = 0
      while (j < words.length && total + words(j).length + (j - i) <= maxWidth) { total += words(j).length; j += 1 }
      val gaps = j - i - 1
      val line =
        if (j == words.length || gaps == 0) {
          val s = words.slice(i, j).mkString(" ")
          s + (" " * (maxWidth - s.length))
        } else {
          val spaces = maxWidth - total; val base = spaces / gaps; val extra = spaces % gaps
          val sb = new StringBuilder
          for (k <- i until j - 1) { sb.append(words(k)); sb.append(" " * (base + (if (k - i < extra) 1 else 0))) }
          sb.append(words(j - 1)); sb.toString
        }
      res :+= line; i = j
    }
    res
  }
  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toArray
    if (lines.nonEmpty) {
      var idx = 0; val t = lines(idx).toInt; idx += 1; val out = scala.collection.mutable.ArrayBuffer[String]()
      for (tc <- 0 until t) { val n = lines(idx).toInt; idx += 1; val words = lines.slice(idx, idx + n).toVector; idx += n; val width = lines(idx).toInt; idx += 1; val ans = justify(words, width); out += ans.length.toString; ans.foreach(s => out += ("|" + s + "|")); if (tc + 1 < t) out += "=" }
      print(out.mkString("\n"))
    }
  }
}
