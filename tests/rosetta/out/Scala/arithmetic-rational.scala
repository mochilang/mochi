object arithmetic_rational {
  def intSqrt(x: Int): Int = {
    if (x < 2) {
      return x
    }
    var left = 1
    var right = x / 2
    var ans = 0
    while (left <= right) {
      val mid = left + (right - left) / 2
      val sq = mid * mid
      if (sq == x) {
        return mid
      }
      if (sq < x) {
        left = mid + 1
        ans = mid
      } else {
        right = mid - 1
      }
    }
    return ans
  }
  
  def sumRecip(n: Int): Int = {
    var s = 1
    val limit = intSqrt(n)
    var f = 2
    while (f <= limit) {
      if (n % f == 0) {
        s = s + n / f
        val f2 = n / f
        if (f2 != f) {
          s += f
        }
      }
      f += 1
    }
    return s
  }
  
  def main() = {
    val nums = List(6, 28, 120, 496, 672, 8128, 30240, 32760, 523776)
    for(n <- nums) {
      val s = sumRecip(n)
      if (s % n == 0) {
        val val = s / n
        var perfect = ""
        if (val == 1) {
          perfect = "perfect!"
        }
        println((((("Sum of recipr. factors of " + n.toString).asInstanceOf[Int] + " = ").asInstanceOf[Int] + val.toString).asInstanceOf[Int] + " exactly ").asInstanceOf[Int] + perfect)
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
