object averages_mean_time_of_day {
  val PI = 3.141592653589793
  def sinApprox(x: Double): Double = {
    var term = x
    var sum = x
    var n = 1
    while (n <= 8) {
      val denom = ((2 * n) * (2 * n + 1)).toDouble
      term = -term * x * x / denom
      sum += term
      n += 1
    }
    return sum
  }
  
  def cosApprox(x: Double): Double = {
    var term = 1
    var sum = 1
    var n = 1
    while (n <= 8) {
      val denom = ((2 * n - 1) * (2 * n)).toDouble
      term = -term * x * x / denom
      sum += term
      n += 1
    }
    return sum
  }
  
  def atanApprox(x: Double): Double = {
    if (x > 1) {
      return PI / 2 - x / (x * x + 0.28)
    }
    if (x < (-1)) {
      return -PI / 2 - x / (x * x + 0.28)
    }
    return x / (1 + 0.28 * x * x)
  }
  
  def atan2Approx(y: Double, x: Double): Double = {
    if (x > 0) {
      return atanApprox(y / x)
    }
    if (x < 0) {
      if (y >= 0) {
        return atanApprox(y / x) + PI
      }
      return atanApprox(y / x) - PI
    }
    if (y > 0) {
      return PI / 2
    }
    if (y < 0) {
      return -PI / 2
    }
    return 0
  }
  
  def digit(ch: String): Int = {
    val digits = "0123456789"
    var i = 0
    while (i < digits.length) {
      if (digits.substring(i, i + 1) == ch) {
        return i
      }
      i += 1
    }
    return 0
  }
  
  def parseTwo(s: String, idx: Int): Int = digit(s.substring(idx, idx + 1)) * 10 + digit(s.substring(idx + 1, idx + 2))
  
  def parseSec(s: String): Double = {
    val h = parseTwo(s, 0)
    val m = parseTwo(s, 3)
    val sec = parseTwo(s, 6)
    return ((h * 60 + m) * 60 + sec).toDouble
  }
  
  def pad(n: Int): String = {
    if (n < 10) {
      return "0" + n.toString
    }
    return n.toString
  }
  
  def meanTime(times: List[String]): String = {
    var ssum = 0
    var csum = 0
    var i = 0
    while (i < times.length) {
      val sec = parseSec((times).apply(i))
      val ang = sec * 2 * PI / 86400
      ssum += sinApprox(ang)
      csum += cosApprox(ang)
      i += 1
    }
    var theta = atan2Approx(ssum, csum)
    var frac = theta / (2 * PI)
    while (frac < 0) {
      frac += 1
    }
    val total = frac * 86400
    val si = total.toInt
    val h = (si / 3600).toInt
    val m = ((si % 3600) / 60).toInt
    val s = (si % 60).toInt
    return (((pad(h) + ":").asInstanceOf[Int] + pad(m)).asInstanceOf[Int] + ":").asInstanceOf[Int] + pad(s)
  }
  
  def main() = {
    val inputs = List("23:00:17", "23:40:20", "00:12:45", "00:17:19")
    println(meanTime(inputs))
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
