object approximate_equality {
  def abs(x: Double): Double = {
    if (x < 0) {
      return -x
    }
    return x
  }
  
  def maxf(a: Double, b: Double): Double = {
    if (a > b) {
      return a
    }
    return b
  }
  
  def isClose(a: Double, b: Double): Boolean = {
    val relTol = 1e-09
    val t = abs(a - b)
    val u = relTol * maxf(abs(a), abs(b))
    return t <= u
  }
  
  def sqrtApprox(x: Double): Double = {
    var guess = x
    var i = 0
    while (i < 10) {
      guess = (guess + x / guess) / 2
      i += 1
    }
    return guess
  }
  
  def main() = {
    val root2 = sqrtApprox(2)
    val pairs = List(List(1.0000000000000002e+14, 1.0000000000000002e+14), List(100.01, 100.011), List(1.0000000000000002e+13 / 10000, 1.0000000000000001e+09), List(0.001, 0.0010000001), List(1.01e-22, 0), List(root2 * root2, 2), List((-root2) * root2, -2), List(1e+17, 1e+17), List(3.141592653589793, 3.141592653589793))
    for(pair <- pairs) {
      val a = (pair).apply(0)
      val b = (pair).apply(1)
      val s = if (isClose(a, b)) "≈" else "≉"
      println(a.toString + " " + s + " " + b.toString)
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
