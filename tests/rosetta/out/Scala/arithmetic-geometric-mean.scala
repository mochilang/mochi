object arithmetic_geometric_mean {
  def abs(x: Double): Double = {
    if (x < 0) {
      return -x
    }
    return x
  }
  
  def sqrtApprox(x: Double): Double = {
    var guess = x
    var i = 0
    while (i < 20) {
      guess = (guess + x / guess) / 2
      i += 1
    }
    return guess
  }
  
  def agm(a: Double, g: Double): Double = {
    val eps = 1e-14
    while (abs(a - g) > abs(a) * eps) {
      val newA = (a + g) / 2
      val newG = sqrtApprox(a * g)
      a = newA
      g = newG
    }
    return a
  }
  
  def main() = {
    println(agm(1, 1 / sqrtApprox(2)).toString)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
