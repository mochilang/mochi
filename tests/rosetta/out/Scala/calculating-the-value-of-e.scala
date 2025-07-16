object calculating_the_value_of_e {
  val epsilon = 1e-15
  def absf(x: Double): Double = {
    if (x < 0) {
      return -x
    }
    return x
  }
  
  def pow10(n: Int): Double = {
    var r: Double = 1
    var i = 0
    while (i < n) {
      r *= 10
      i += 1
    }
    return r
  }
  
  def formatFloat(f: Double, prec: Int): String = {
    val scale = pow10(prec)
    val scaled = (f * scale) + 0.5
    var n = (scaled.toInt)
    var digits = n.toString
    while (digits.length <= prec) {
      digits = "0" + digits
    }
    val intPart = digits.substring(0, digits.length - prec)
    val fracPart = digits.substring(digits.length - prec, digits.length)
    return intPart + "." + fracPart
  }
  
  def main(args: Array[String]): Unit = {
    var factval = 1
    var e: Double = 2
    var n = 2
    var term: Double = 1
    while (true) {
      factval *= n
      n += 1
      term = 1 / (factval.toDouble)
      e += term
      if (absf(term) < epsilon) {
        return
      }
    }
    println("e = " + formatFloat(e, 15))
  }
}
