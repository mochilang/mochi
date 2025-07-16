object anti_primes {
  def countDivisors(n: Int): Int = {
    if (n < 2) {
      return 1
    }
    var count = 2
    var i = 2
    while (i <= n / 2) {
      if (n % i == 0) {
        count += 1
      }
      i += 1
    }
    return count
  }
  
  def main() = {
    println("The first 20 anti-primes are:")
    var maxDiv = 0
    var count = 0
    var n = 1
    var line = ""
    while (count < 20) {
      val d = countDivisors(n)
      if (d > maxDiv) {
        line = (line + n.toString).asInstanceOf[Int] + " "
        maxDiv = d
        count += 1
      }
      n += 1
    }
    line = line.substring(0, line.length - 1)
    println(line)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
