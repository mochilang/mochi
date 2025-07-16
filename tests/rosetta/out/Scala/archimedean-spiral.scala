object archimedean_spiral {
  val PI = 3.141592653589793
  def sinApprox(x: Double): Double = {
    var term = x
    var sum = x
    var n = 1
    while (n <= 10) {
      val denom = ((2 * n) * (2 * n + 1)).toDouble
      term = -term * x * x / denom
      sum += term
      n += 1
    }
    return sum
  }
  
  def cosApprox(x: Double): Double = {
    var term = 1
    var sum = 1
    var n = 1
    while (n <= 10) {
      val denom = ((2 * n - 1) * (2 * n)).toDouble
      term = -term * x * x / denom
      sum += term
      n += 1
    }
    return sum
  }
  
  def main(args: Array[String]): Unit = {
    val degreesIncr = 0.1 * PI / 180
    val turns = 2
    val stop = 360 * turns * 10 * degreesIncr
    val width = 600
    val centre = width / 2
    val a = 1
    val b = 20
    var theta = 0
    var count = 0
    while (theta < stop) {
      val r = a + b * theta
      val x = r * cosApprox(theta)
      val y = r * sinApprox(theta)
      if (count % 100 == 0) {
        println((centre + x.toString + ",").asInstanceOf[Int] + centre - y.toString)
      }
      theta += degreesIncr
      count += 1
    }
  }
}
