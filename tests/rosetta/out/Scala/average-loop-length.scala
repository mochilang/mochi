object average_loop_length {
  def absf(x: Double): Double = {
    if (x < 0) {
      return -x
    }
    return x
  }
  
  def floorf(x: Double): Double = (x.toInt).toDouble
  
  def indexOf(s: String, ch: String): Int = {
    var i = 0
    while (i < s.length) {
      if (s.substring(i, i + 1) == ch) {
        return i
      }
      i += 1
    }
    return -1
  }
  
  def fmtF(x: Double): String = {
    var y = floorf(x * 10000 + 0.5) / 10000
    var s = y.toString
    var dot = indexOf(s, ".")
    if (dot == 0 - 1) {
      s += ".0000"
    } else {
      var decs = s.length - dot - 1
      if (decs > 4) {
        s = s.substring(0, dot + 5)
      } else {
        while (decs < 4) {
          s += "0"
          decs += 1
        }
      }
    }
    return s
  }
  
  def padInt(n: Int, width: Int): String = {
    var s = n.toString
    while (s.length < width) {
      s = " " + s
    }
    return s
  }
  
  def padFloat(x: Double, width: Int): String = {
    var s = fmtF(x)
    while (s.length < width) {
      s = " " + s
    }
    return s
  }
  
  def avgLen(n: Int): Double = {
    val tests = 10000
    var sum = 0
    var seed = 1
    var t = 0
    while (t < tests) {
      var visited: List[Boolean] = scala.collection.mutable.ArrayBuffer[Any]()
      var i = 0
      while (i < n) {
        visited = visited :+ false
        i += 1
      }
      var x = 0
      while (!(visited).apply(x)) {
        visited(x) = true
        sum += 1
        seed = (seed * 1664525 + 1013904223) % 2147483647
        x = seed % n
      }
      t += 1
    }
    return (sum.toDouble) / tests
  }
  
  def ana(n: Int): Double = {
    var nn = n.toDouble
    var term = 1
    var sum = 1
    var i = nn - 1
    while (i >= 1) {
      term *= (i / nn)
      sum += term
      i -= 1
    }
    return sum
  }
  
  def main() = {
    val nmax = 20
    println(" N    average    analytical    (error)")
    println("===  =========  ============  =========")
    var n = 1
    while (n <= nmax) {
      val a = avgLen(n)
      val b = ana(n)
      val err = absf(a - b) / b * 100
      var line = padInt(n, 3) + "  " + padFloat(a, 9) + "  " + padFloat(b, 12) + "  (" + padFloat(err, 6) + "%)"
      println(line)
      n += 1
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
