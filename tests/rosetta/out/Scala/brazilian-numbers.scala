object brazilian_numbers {
  def sameDigits(n: Int, b: Int): Boolean = {
    var f = n % b
    n = (n / b).toInt
    while (n > 0) {
      if (n % b != f) {
        return false
      }
      n = (n / b).toInt
    }
    return true
  }
  
  def isBrazilian(n: Int): Boolean = {
    if (n < 7) {
      return false
    }
    if (n % 2 == 0 && n >= 8) {
      return true
    }
    var b = 2
    while (b < n - 1) {
      if (sameDigits(n, b)) {
        return true
      }
      b += 1
    }
    return false
  }
  
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
  
  def main() = {
    var kinds = scala.collection.mutable.ArrayBuffer(" ", " odd ", " prime ")
    for(kind <- kinds) {
      println("First 20" + kind + "Brazilian numbers:")
      var c = 0
      var n = 7
      while (true) {
        if (isBrazilian(n)) {
          println(n.toString + " ")
          c += 1
          if (c == 20) {
            println("\n")
            return
          }
        }
        if (kind == " ") {
          n += 1
        } else {
          if (kind == " odd ") {
            n += 2
          } else {
            while (true) {
              n += 2
              if (isPrime(n)) {
                return
              }
            }
          }
        }
      }
    }
    var n = 7
    var c = 0
    while (c < 100000) {
      if (isBrazilian(n)) {
        c += 1
      }
      n += 1
    }
    println("The 100,000th Brazilian number: " + n - 1.toString)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
