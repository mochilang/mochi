import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Iterator.continually(StdIn.readLine()).takeWhile(_ != null).map(_.trim).filter(_.nonEmpty).toArray
    if (lines.isEmpty) return
    val t = lines(0).toInt
    var idx = 1
    val out = collection.mutable.ArrayBuffer.empty[String]
    for (_ <- 0 until t) {
      val parts = lines(idx).split("\\s+")
      idx += 1
      val r = parts(0).toInt
      val image = lines.slice(idx, idx + r)
      idx += r
      idx += 1
      var top = r
      var bottom = -1
      var left = image(0).length
      var right = -1
      for (i <- image.indices; j <- image(i).indices if image(i)(j) == '1') {
        top = math.min(top, i)
        bottom = math.max(bottom, i)
        left = math.min(left, j)
        right = math.max(right, j)
      }
      out += ((bottom - top + 1) * (right - left + 1)).toString
    }
    print(out.mkString("\n\n"))
  }
}
