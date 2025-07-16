object chernicks_carmichael_numbers {
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
  
  def bigTrim(a: List[Int]): List[Int] = {
    var n = a.size
    while (n > 1 && (a).apply(n - 1) == 0) {
      a = a.slice(0, n - 1)
      n -= 1
    }
    return a
  }
  
  def bigFromInt(x: Int): List[Int] = {
    if (x == 0) {
      return List(0)
    }
    var digits: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var n = x
    while (n > 0) {
      digits = digits :+ n % 10
      n /= 10
    }
    return digits
  }
  
  def bigMulSmall(a: List[Int], m: Int): List[Int] = {
    if (m == 0) {
      return List(0)
    }
    var res: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var carry = 0
    var i = 0
    while (i < a.size) {
      var prod = (a).apply(i) * m + carry
      res = res :+ prod % 10
      carry = prod / 10
      i += 1
    }
    while (carry > 0) {
      res = res :+ carry % 10
      carry /= 10
    }
    return bigTrim(res)
  }
  
  def bigToString(a: List[Int]): String = {
    var s = ""
    var i = a.size - 1
    while (i >= 0) {
      s += (a).apply(i).toString
      i -= 1
    }
    return s
  }
  
  def pow2(k: Int): Int = {
    var r = 1
    var i = 0
    while (i < k) {
      r *= 2
      i += 1
    }
    return r
  }
  
  def ccFactors(n: Int, m: Int): List[Int] = {
    var p = 6 * m + 1
    if (!isPrime(p)) {
      return List()
    }
    var prod = bigFromInt(p)
    p = 12 * m + 1
    if (!isPrime(p)) {
      return List()
    }
    prod = bigMulSmall(prod, p)
    var i = 1
    while (i <= n - 2) {
      p = (pow2(i) * 9 * m) + 1
      if (!isPrime(p)) {
        return List()
      }
      prod = bigMulSmall(prod, p)
      i += 1
    }
    return prod
  }
  
  def ccNumbers(start: Int, end: Int) = {
    var n = start
    while (n <= end) {
      var m = 1
      if (n > 4) {
        m = pow2(n - 4)
      }
      while (true) {
        val num = ccFactors(n, m)
        if (num.size > 0) {
          println((("a(" + n.toString).asInstanceOf[Int] + ") = ").asInstanceOf[Int] + bigToString(num))
          return
        }
        if (n <= 4) {
          m += 1
        } else {
          m += pow2(n - 4)
        }
      }
      n += 1
    }
  }
  
  def main(args: Array[String]): Unit = {
    ccNumbers(3, 9)
  }
}
