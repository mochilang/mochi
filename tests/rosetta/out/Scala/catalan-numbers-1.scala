object catalan_numbers_1 {
  def binom(n: Int, k: Int): Int = {
    if (k < 0 || k > n) {
      return 0
    }
    var kk = k
    if (kk > n - kk) {
      kk = n - kk
    }
    var res = 1
    var i = 0
    while (i < kk) {
      res = (res * (n - i))
      i += 1
      res = (res / i).toInt
    }
    return res
  }
  
  def catalan(n: Int): Int = (binom(2 * n, n) / (n + 1)).toInt
  
  def main() = {
    for(i <- 0 until 15) {
      println(catalan(i).toString)
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
