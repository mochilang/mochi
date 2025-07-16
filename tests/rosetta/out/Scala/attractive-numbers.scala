object attractive_numbers {
  def isPrime(n: Int): Boolean = {
    if (n < 2) {
      return false
    }
    if (n % 2 == 0) {
      return n == 2
    }
    if (n % 3 == 0) {
      return n == 3
    }
    var d = 5
    while (d * d <= n) {
      if (n % d == 0) {
        return false
      }
      d += 2
      if (n % d == 0) {
        return false
      }
      d += 4
    }
    return true
  }
  
  def countPrimeFactors(n: Int): Int = {
    if (n == 1) {
      return 0
    }
    if (isPrime(n)) {
      return 1
    }
    var count = 0
    var f = 2
    while (true) {
      if (n % f == 0) {
        count += 1
        n /= f
        if (n == 1) {
          return count
        }
        if (isPrime(n)) {
          f = n
        }
      } else {
        if (f >= 3) {
          f += 2
        } else {
          f = 3
        }
      }
    }
    return count
  }
  
  def pad4(n: Int): String = {
    var s = n.toString
    while (s.length < 4) {
      s = " " + s
    }
    return s
  }
  
  def main() = {
    val max = 120
    println(("The attractive numbers up to and including " + max.toString).asInstanceOf[Int] + " are:")
    var count = 0
    var line = ""
    var lineCount = 0
    var i = 1
    while (i <= max) {
      val c = countPrimeFactors(i)
      if (isPrime(c)) {
        line += pad4(i)
        count += 1
        lineCount += 1
        if (lineCount == 20) {
          println(line)
          line = ""
          lineCount = 0
        }
      }
      i += 1
    }
    if (lineCount > 0) {
      println(line)
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
