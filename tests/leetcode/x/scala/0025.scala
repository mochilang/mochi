object Main {
  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toArray
    if (lines.nonEmpty) {
      var idx = 0
      val t = lines(idx).trim.toInt; idx += 1
      val out = collection.mutable.ArrayBuffer[String]()
      for (_ <- 0 until t) {
        val n = if (idx < lines.length) lines(idx).trim.toInt else 0; idx += 1
        val arr = collection.mutable.ArrayBuffer[Int]()
        for (_ <- 0 until n) { arr += (if (idx < lines.length) lines(idx).trim.toInt else 0); idx += 1 }
        val k = if (idx < lines.length) lines(idx).trim.toInt else 1; idx += 1
        var i = 0
        while (i + k <= arr.length) {
          var l = i; var r = i + k - 1
          while (l < r) { val tmp = arr(l); arr(l) = arr(r); arr(r) = tmp; l += 1; r -= 1 }
          i += k
        }
        out += ("[" + arr.mkString(",") + "]")
      }
      print(out.mkString("\n"))
    }
  }
}
