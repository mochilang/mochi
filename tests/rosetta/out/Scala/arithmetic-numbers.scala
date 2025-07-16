object arithmetic_numbers {
  def sieve(limit: Int): List[Int] = {
    var spf: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i <= limit) {
      spf = spf :+ 0
      i += 1
    }
    i = 2
    while (i <= limit) {
      if ((spf).apply(i) == 0) {
        spf(i) = i
        if (i * i <= limit) {
          var j = i * i
          while (j <= limit) {
            if ((spf).apply(j) == 0) {
              spf(j) = i
            }
            j += i
          }
        }
      }
      i += 1
    }
    return spf
  }
  
  def primesFrom(spf: List[Int], limit: Int): List[Int] = {
    var primes: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 3
    while (i <= limit) {
      if ((spf).apply(i) == i) {
        primes = primes :+ i
      }
      i += 1
    }
    return primes
  }
  
  def pad3(n: Int): String = {
    var s = n.toString
    while (s.length < 3) {
      s = " " + s
    }
    return s
  }
  
  def commatize(n: Int): String = {
    var s = n.toString
    var out = ""
    var i = s.length - 1
    var c = 0
    while (i >= 0) {
      out = s.substring(i, i + 1) + out
      c += 1
      if (c % 3 == 0 && i > 0) {
        out = "," + out
      }
      i -= 1
    }
    return out
  }
  
  def primeCount(primes: List[Int], last: Int, spf: List[Int]): Int = {
    var lo = 0
    var hi = primes.length
    while (lo < hi) {
      var mid = ((lo + hi) / 2).toInt
      if ((primes).apply(mid) < last) {
        lo = mid + 1
      } else {
        hi = mid
      }
    }
    var count = lo + 1
    if ((spf).apply(last) != last) {
      count -= 1
    }
    return count
  }
  
  def arithmeticNumbers(limit: Int, spf: List[Int]): List[Int] = {
    var arr: List[Int] = scala.collection.mutable.ArrayBuffer(1)
    var n = 3
    while (arr.length < limit) {
      if ((spf).apply(n) == n) {
        arr = arr :+ n
      } else {
        var x = n
        var sigma = 1
        var tau = 1
        while (x > 1) {
          var p = (spf).apply(x)
          if (p == 0) {
            p = x
          }
          var cnt = 0
          var power = p
          var sum = 1
          while (x % p == 0) {
            x /= p
            cnt += 1
            sum += power
            power *= p
          }
          sigma *= sum
          tau *= (cnt + 1)
        }
        if (sigma % tau == 0) {
          arr = arr :+ n
        }
      }
      n += 1
    }
    return arr
  }
  
  def main() = {
    val limit = 1228663
    val spf = sieve(limit)
    val primes = primesFrom(spf, limit)
    val arr = arithmeticNumbers(1000000, spf)
    println("The first 100 arithmetic numbers are:")
    var i = 0
    while (i < 100) {
      var line = ""
      var j = 0
      while (j < 10) {
        line += pad3((arr).apply(i + j))
        if (j < 9) {
          line += " "
        }
        j += 1
      }
      println(line)
      i += 10
    }
    for(x <- List(1000, 10000, 100000, 1000000)) {
      val last = (arr).apply(x - 1)
      val lastc = commatize(last)
      println((("\nThe " + commatize(x)).asInstanceOf[Int] + "th arithmetic number is: ").asInstanceOf[Int] + lastc)
      val pc = primeCount(primes, last, spf)
      val comp = x - pc - 1
      println(((("The count of such numbers <= " + lastc).asInstanceOf[Int] + " which are composite is ").asInstanceOf[Int] + commatize(comp)).asInstanceOf[Int] + ".")
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
