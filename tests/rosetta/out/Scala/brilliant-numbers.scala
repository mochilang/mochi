object brilliant_numbers {
  def primesUpTo(n: Int): List[Int] = {
    var sieve: List[Boolean] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i <= n) {
      sieve = sieve :+ true
      i += 1
    }
    var p = 2
    while (p * p <= n) {
      if ((sieve).apply(p)) {
        var m = p * p
        while (m <= n) {
          sieve(m) = false
          m += p
        }
      }
      p += 1
    }
    var res: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var x = 2
    while (x <= n) {
      if ((sieve).apply(x)) {
        res = res :+ x
      }
      x += 1
    }
    return res
  }
  
  def sortInts(xs: List[Int]): List[Int] = {
    var res: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var tmp = xs
    while (tmp.length > 0) {
      var min = (tmp).apply(0)
      var idx = 0
      var i = 1
      while (i < tmp.length) {
        if ((tmp).apply(i) < min) {
          min = (tmp).apply(i)
          idx = i
        }
        i += 1
      }
      res = res :+ min
      var out: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
      var j = 0
      while (j < tmp.length) {
        if (j != idx) {
          out = out :+ (tmp).apply(j)
        }
        j += 1
      }
      tmp = out
    }
    return res
  }
  
  def commatize(n: Int): String = {
    var s = n.toString
    var i = s.length - 3
    while (i >= 1) {
      s = s.substring(0, i) + "," + s.substring(i, s.length)
      i -= 3
    }
    return s
  }
  
  def getBrilliant(digits: Int, limit: Int, countOnly: Boolean): Map[String, any] = {
    var brilliant: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var count = 0
    var pow = 1
    var next = 999999999999999
    var k = 1
    while (k <= digits) {
      var s: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
      for(p <- primes) {
        if (p >= pow * 10) {
          return
        }
        if (p > pow) {
          s = s :+ p
        }
      }
      var i = 0
      while (i < s.length) {
        var j = i
        while (j < s.length) {
          var prod = (s).apply(i) * (s).apply(j)
          if (prod < limit) {
            if (countOnly) {
              count += 1
            } else {
              brilliant = brilliant :+ prod
            }
          } else {
            if (prod < next) {
              next = prod
            }
            return
          }
          j += 1
        }
        i += 1
      }
      pow *= 10
      k += 1
    }
    if (countOnly) {
      return Map("bc" -> count, "next" -> next)
    }
    return Map("bc" -> brilliant, "next" -> next)
  }
  
  def main() = {
    println("First 100 brilliant numbers:")
    val r = getBrilliant(2, 10000, false)
    var br = sortInts((r).apply("bc"))
    br = br.slice(0, 100)
    var i = 0
    while (i < br.length) {
      println(s"${(br).apply(i).toString.reverse.padTo(4, ' ').reverse + " "} ${false}")
      if ((i + 1) % 10 == 0) {
        println(s" ${true}")
      }
      i += 1
    }
    println(s" ${true}")
    var k = 1
    while (k <= 13) {
      val limit = pow(10, k)
      val r2 = getBrilliant(k, limit, true)
      val total = (r2).apply("bc")
      val next = (r2).apply("next")
      val climit = commatize(limit)
      val ctotal = commatize((total).asInstanceOf[Int] + 1)
      val cnext = commatize(next)
      println("First >= " + climit.padStart(18, " ") + " is " + ctotal.padStart(14, " ") + " in the series: " + cnext.padStart(18, " "))
      k += 1
    }
  }
  
  def main(args: Array[String]): Unit = {
    var primes = primesUpTo(3200000)
  }
}
