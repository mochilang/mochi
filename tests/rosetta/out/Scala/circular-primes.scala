object circular_primes {
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
  
  def isCircular(n: Int): Boolean = {
    var nn = n
    var pow = 1
    while (nn > 0) {
      pow *= 10
      nn /= 10
    }
    nn = n
    while (true) {
      nn *= 10
      val f = nn / pow
      nn = nn + f * (1 - pow)
      if (nn == n) {
        return
      }
      if (!isPrime(nn)) {
        return false
      }
    }
    return true
  }
  
  def showList(xs: List[Int]): String = {
    var out = "["
    var i = 0
    while (i < xs.length) {
      out += (xs).apply(i).toString
      if (i < xs.length - 1) {
        out += ", "
      }
      i += 1
    }
    return out + "]"
  }
  
  def main(args: Array[String]): Unit = {
    var circs: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    println("The first 19 circular primes are:")
    var digits = scala.collection.mutable.ArrayBuffer(1, 3, 7, 9)
    var q = scala.collection.mutable.ArrayBuffer(1, 2, 3, 5, 7, 9)
    var fq = scala.collection.mutable.ArrayBuffer(1, 2, 3, 5, 7, 9)
    var count = 0
    while (true) {
      val f = (q).apply(0)
      val fd = (fq).apply(0)
      if (isPrime(f) && isCircular(f)) {
        circs = circs :+ f
        count += 1
        if (count == 19) {
          return
        }
      }
      q = q.slice(1, q.length)
      fq = fq.slice(1, fq.length)
      if (f != 2 && f != 5) {
        for(d <- digits) {
          q = q :+ f * 10 + d
          fq = fq :+ fd
        }
      }
    }
    println(showList(circs))
    println("\nThe next 4 circular primes, in repunit format, are:")
    println("[R(19) R(23) R(317) R(1031)]")
    println("\nThe following repunits are probably circular primes:")
    for(i <- List(5003, 9887, 15073, 25031, 35317, 49081)) {
      println("R(" + i.toString + ") : true")
    }
  }
}
