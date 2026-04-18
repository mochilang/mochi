object Main {
  def firstMissingPositive(nums: Array[Int]): Int = {
    val n = nums.length
    var i = 0
    while (i < n) {
      val v = nums(i)
      if (v >= 1 && v <= n && nums(v - 1) != v) {
        val tmp = nums(i)
        nums(i) = nums(v - 1)
        nums(v - 1) = tmp
      } else {
        i += 1
      }
    }
    i = 0
    while (i < n) {
      if (nums(i) != i + 1) return i + 1
      i += 1
    }
    n + 1
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().map(_.trim).toArray
    if (lines.nonEmpty && lines(0).nonEmpty) {
      var idx = 0
      val t = lines(idx).toInt
      idx += 1
      val out = scala.collection.mutable.ArrayBuffer[String]()
      for (_ <- 0 until t) {
        val n = lines(idx).toInt
        idx += 1
        val nums = Array.ofDim[Int](n)
        for (i <- 0 until n) { nums(i) = lines(idx).toInt; idx += 1 }
        out += firstMissingPositive(nums).toString
      }
      print(out.mkString("\n"))
    }
  }
}
