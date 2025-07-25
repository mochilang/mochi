object averages_pythagorean_means {
  def powf(base: Double, exp: Int): Double = {
    var result = 1
    var i = 0
    while (i < exp) {
      result *= base
      i += 1
    }
    return result
  }
  
  def nthRoot(x: Double, n: Int): Double = {
    var low = 0
    var high = x
    var i = 0
    while (i < 60) {
      val mid = (low + high) / 2
      if (powf(mid, n) > x) {
        high = mid
      } else {
        low = mid
      }
      i += 1
    }
    return low
  }
  
  def main() = {
    var sum = 0
    var sumRecip = 0
    var prod = 1
    var n = 1
    while (n <= 10) {
      val f = n.toDouble
      sum += f
      sumRecip = sumRecip + 1 / f
      prod *= f
      n += 1
    }
    val count = 10
    val a = sum / count
    val g = nthRoot(prod, 10)
    val h = count / sumRecip
    println("A: " + a.toString + " G: " + g.toString + " H: " + h.toString)
    println("A >= G >= H: " + a >= g && g >= h.toString)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
