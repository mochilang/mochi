object averages_arithmetic_mean {
  def mean(v: List[Double]): Map[String, any] = {
    if (v.length == 0) {
      return Map("ok" -> false)
    }
    var sum = 0
    var i = 0
    while (i < v.length) {
      sum += (v).apply(i)
      i += 1
    }
    return Map("ok" -> true, "mean" -> sum / (v.length.toDouble))
  }
  
  def main() = {
    val sets = List(List(), List(3, 1, 4, 1, 5, 9), List(1e+20, 3, 1, 4, 1, 5, 9, -1e+20), List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, 0.11), List(10, 20, 30, 40, 50, -100, 4.7, -1100))
    for(v <- sets) {
      println("Vector: " + v.toString)
      val r = mean(v)
      if ((r).apply("ok") != null) {
        println("Mean of " + v.length.toString + " numbers is " + (r).apply("mean").toString)
      } else {
        println("Mean undefined")
      }
      println("")
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
