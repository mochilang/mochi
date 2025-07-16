object aks_test_for_primes {
  def poly(p: Int): String = {
    var s: String = ""
    var coef: Int = 1
    var i = p
    if (coef != 1) {
      s += coef.toString
    }
    while (i > 0) {
      s += "x"
      if (i != 1) {
        s = (s + "^").asInstanceOf[Int] + i.toString
      }
      coef = (coef * i / (p - i + 1)).toInt
      var d = coef
      if ((p - (i - 1)) % 2 == 1) {
        d = -d
      }
      if (d < 0) {
        s = (s + " - ").asInstanceOf[Int] + -d.toString
      } else {
        s = (s + " + ").asInstanceOf[Int] + d.toString
      }
      i -= 1
    }
    if (s == "") {
      s = "1"
    }
    return s
  }
  
  def aks(n: Int): Boolean = {
    if (n < 2) {
      return false
    }
    var c: Int = n
    var i = 1
    while (i < n) {
      if (c % n != 0) {
        return false
      }
      c = (c * (n - i) / (i + 1)).toInt
      i += 1
    }
    return true
  }
  
  def main() = {
    var p = 0
    while (p <= 7) {
      println((p.toString + ":  ").asInstanceOf[Int] + poly(p))
      p += 1
    }
    var first = true
    p = 2
    var line: String = ""
    while (p < 50) {
      if (aks(p)) {
        if (first) {
          line += p.toString
          first = false
        } else {
          line = (line + " ").asInstanceOf[Int] + p.toString
        }
      }
      p += 1
    }
    println(line)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
