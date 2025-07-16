object achilles_numbers {
  def pow10(exp: Int): Int = {
    var n = 1
    var i = 0
    while (i < exp) {
      n *= 10
      i += 1
    }
    return n
  }
  
  def totient(n: Int): Int = {
    var tot = n
    var nn = n
    var i = 2
    while (i * i <= nn) {
      if (nn % i == 0) {
        while (nn % i == 0) {
          nn /= i
        }
        tot = tot - tot / i
      }
      if (i == 2) {
        i = 1
      }
      i += 2
    }
    if (nn > 1) {
      tot = tot - tot / nn
    }
    return tot
  }
  
  def getPerfectPowers(maxExp: Int) = {
    val upper = pow10(maxExp)
    var i = 2
    while (i * i < upper) {
      var p = i * i
      while (true) {
        p *= i
        if (p >= upper) {
          return
        }
        pps(p) = true
      }
      i += 1
    }
  }
  
  def getAchilles(minExp: Int, maxExp: Int): Map[Int, Boolean] = {
    val lower = pow10(minExp)
    val upper = pow10(maxExp)
    var achilles: Map[Int, Boolean] = scala.collection.mutable.Map()
    var b = 1
    while (b * b * b < upper) {
      val b3 = b * b * b
      var a = 1
      while (true) {
        val p = b3 * a * a
        if (p >= upper) {
          return
        }
        if (p >= lower) {
          if (!(pps.contains(p))) {
            achilles(p) = true
          }
        }
        a += 1
      }
      b += 1
    }
    return achilles
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
      res += List(min)
      var out: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
      var j = 0
      while (j < tmp.length) {
        if (j != idx) {
          out += List((tmp).apply(j))
        }
        j += 1
      }
      tmp = out
    }
    return res
  }
  
  def pad(n: Int, width: Int): String = {
    var s = n.toString
    while (s.length < width) {
      s = " " + s
    }
    return s
  }
  
  def main() = {
    val maxDigits = 15
    getPerfectPowers(maxDigits)
    val achSet = getAchilles(1, 5)
    var ach: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    for(k <- achSet("keys")()) {
      ach += List(k)
    }
    ach = sortInts(ach)
    println("First 50 Achilles numbers:")
    var i = 0
    while (i < 50) {
      var line = ""
      var j = 0
      while (j < 10) {
        line += pad((ach).apply(i), 4)
        if (j < 9) {
          line += " "
        }
        i += 1
        j += 1
      }
      println(line)
    }
    println("\nFirst 30 strong Achilles numbers:")
    var strong: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var count = 0
    var idx = 0
    while (count < 30) {
      val tot = totient((ach).apply(idx))
      if (achSet.contains(tot)) {
        strong += List((ach).apply(idx))
        count += 1
      }
      idx += 1
    }
    i = 0
    while (i < 30) {
      var line = ""
      var j = 0
      while (j < 10) {
        line += pad((strong).apply(i), 5)
        if (j < 9) {
          line += " "
        }
        i += 1
        j += 1
      }
      println(line)
    }
    println("\nNumber of Achilles numbers with:")
    val counts = List(1, 12, 47, 192, 664, 2242, 7395, 24008, 77330, 247449, 788855, 2508051, 7960336, 25235383)
    var d = 2
    while (d <= maxDigits) {
      val c = (counts).apply(d - 2)
      println((pad(d, 2) + " digits: ").asInstanceOf[Int] + c.toString)
      d += 1
    }
  }
  
  def main(args: Array[String]): Unit = {
    var pps: Map[Int, Boolean] = scala.collection.mutable.Map()
    main()
  }
}
