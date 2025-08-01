object averages_root_mean_square {
  def sqrtApprox(x: Double): Double = {
    var guess = x
    var i = 0
    while (i < 20) {
      guess = (guess + x / guess) / 2
      i += 1
    }
    return guess
  }
  
  def main(args: Array[String]): Unit = {
    val n = 10
    var sum = 0
    var x = 1
    while (x <= n) {
      sum = sum + (x.toDouble) * (x.toDouble)
      x += 1
    }
    val rms = sqrtApprox(sum / (n.toDouble))
    println(rms.toString)
  }
}
