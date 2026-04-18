object Main {
  def getPermutation(n: Int, kInput: Int): String = {
    val digits = scala.collection.mutable.ArrayBuffer[String]()
    for (i <- 1 to n) digits += i.toString
    val fact = Array.fill(n + 1)(1)
    for (i <- 1 to n) fact(i) = fact(i - 1) * i
    var k = kInput - 1
    val out = new StringBuilder
    for (rem <- n to 1 by -1) {
      val block = fact(rem - 1)
      val idx = k / block
      k %= block
      out.append(digits.remove(idx))
    }
    out.toString
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().map(_.trim).toArray
    if (lines.nonEmpty && lines(0).nonEmpty) {
      var idx = 0
      val t = lines(idx).toInt
      idx += 1
      val out = scala.collection.mutable.ArrayBuffer[String]()
      for (_ <- 0 until t) {
        val n = lines(idx).toInt; idx += 1
        val k = lines(idx).toInt; idx += 1
        out += getPermutation(n, k)
      }
      print(out.mkString("\n"))
    }
  }
}
