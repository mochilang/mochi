object Main {
  def median(a: Vector[Int], b: Vector[Int]): Double = {
    val m = (a ++ b).sorted
    if (m.length % 2 == 1) m(m.length / 2).toDouble else (m(m.length / 2 - 1) + m(m.length / 2)) / 2.0
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toVector
    if (lines.nonEmpty) {
      val t = lines(0).trim.toInt
      var idx = 1
      val out = (0 until t).map { _ =>
        val n = lines(idx).trim.toInt; idx += 1
        val a = (0 until n).map(_ => { val v = lines(idx).trim.toInt; idx += 1; v }).toVector
        val m = lines(idx).trim.toInt; idx += 1
        val b = (0 until m).map(_ => { val v = lines(idx).trim.toInt; idx += 1; v }).toVector
        f"${median(a, b)}%.1f"
      }
      print(out.mkString("\n"))
    }
  }
}
