object Main {
  def solve(values: Array[Int], target: Double, k: Int): Array[Int] = {
    var right = 0
    while (right < values.length && values(right) < target) right += 1
    var left = right - 1
    val ans = new scala.collection.mutable.ArrayBuffer[Int]()
    while (ans.length < k) {
      if (left < 0) { ans += values(right); right += 1 }
      else if (right >= values.length) { ans += values(left); left -= 1 }
      else if (math.abs(values(left) - target) <= math.abs(values(right) - target)) { ans += values(left); left -= 1 }
      else { ans += values(right); right += 1 }
    }
    ans.toArray
  }

  def main(args: Array[String]): Unit = {
    val toks = io.Source.stdin.getLines().flatMap(_.trim.split("\\s+")).filter(_.nonEmpty).toArray
    if (toks.isEmpty) return
    var idx = 0
    val t = toks(idx).toInt
    idx += 1
    val blocks = new scala.collection.mutable.ArrayBuffer[String]()
    for (_ <- 0 until t) {
      val n = toks(idx).toInt
      idx += 1
      val values = Array.tabulate(n)(_ => { val v = toks(idx).toInt; idx += 1; v })
      val target = toks(idx).toDouble
      idx += 1
      val k = toks(idx).toInt
      idx += 1
      val ans = solve(values, target, k)
      blocks += ((Array(ans.length.toString) ++ ans.map(_.toString)).mkString("\n"))
    }
    print(blocks.mkString("\n\n"))
  }
}
