object biorhythms {
  val PI: Double = 3.141592653589793
  val TWO_PI: Double = 6.283185307179586
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
  
  def floor(x: Double): Double = {
    var i = x.toInt
    if ((i.toDouble) > x) {
      i -= 1
    }
    return i.toDouble
  }
  
  def absFloat(x: Double): Double = {
    if (x < 0) {
      return -x
    }
    return x
  }
  
  def absInt(n: Int): Int = {
    if (n < 0) {
      return -n
    }
    return n
  }
  
  def parseIntStr(str: String): Int = {
    var i = 0
    var neg = false
    if (str.length > 0 && str.substring(0, 1) == "-") {
      neg = true
      i = 1
    }
    var n = 0
    val digits = Map("0" -> 0, "1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5, "6" -> 6, "7" -> 7, "8" -> 8, "9" -> 9)
    while (i < str.length) {
      n = n * 10 + (digits).apply(str.substring(i, i + 1))
      i += 1
    }
    if (neg) {
      n = -n
    }
    return n
  }
  
  def parseDate(s: String): List[Int] = {
    val y = parseIntStr(s.substring(0, 4))
    val m = parseIntStr(s.substring(5, 7))
    val d = parseIntStr(s.substring(8, 10))
    return List(y, m, d)
  }
  
  def leap(y: Int): Boolean = {
    if (y % 400 == 0) {
      return true
    }
    if (y % 100 == 0) {
      return false
    }
    return y % 4 == 0
  }
  
  def daysInMonth(y: Int, m: Int): Int = {
    val feb = if (leap(y)) 29 else 28
    val lengths = List(31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    return (lengths).apply(m - 1)
  }
  
  def addDays(y: Int, m: Int, d: Int, n: Int): List[Int] = {
    var yy = y
    var mm = m
    var dd = d
    if (n >= 0) {
      var i = 0
      while (i < n) {
        dd += 1
        if (dd > daysInMonth(yy, mm)) {
          dd = 1
          mm += 1
          if (mm > 12) {
            mm = 1
            yy += 1
          }
        }
        i += 1
      }
    } else {
      var i = 0
      while (i > n) {
        dd -= 1
        if (dd < 1) {
          mm -= 1
          if (mm < 1) {
            mm = 12
            yy -= 1
          }
          dd = daysInMonth(yy, mm)
        }
        i -= 1
      }
    }
    return List(yy, mm, dd)
  }
  
  def pad2(n: Int): String = {
    if (n < 10) {
      return "0" + n.toString
    }
    return n.toString
  }
  
  def dateString(y: Int, m: Int, d: Int): String = y.toString + "-" + pad2(m) + "-" + pad2(d)
  
  def day(y: Int, m: Int, d: Int): Int = {
    val part1 = 367 * y
    val part2 = ((7 * ((y + ((m + 9) / 12)).toInt)) / 4).toInt
    val part3 = ((275 * m) / 9).toInt
    return part1 - part2 + part3 + d - 730530
  }
  
  def biorhythms(birth: String, target: String) = {
    val bparts = parseDate(birth)
    val by = (bparts).apply(0)
    val bm = (bparts).apply(1)
    val bd = (bparts).apply(2)
    val tparts = parseDate(target)
    val ty = (tparts).apply(0)
    val tm = (tparts).apply(1)
    val td = (tparts).apply(2)
    val diff = absInt(day(ty, tm, td) - day(by, bm, bd))
    println("Born " + birth + ", Target " + target)
    println("Day " + diff.toString)
    val cycles = List("Physical day ", "Emotional day", "Mental day   ")
    val lengths = List(23, 28, 33)
    val quadrants = List(List("up and rising", "peak"), List("up but falling", "transition"), List("down and falling", "valley"), List("down but rising", "transition"))
    var i = 0
    while (i < 3) {
      val length = (lengths).apply(i)
      val cycle = (cycles).apply(i)
      val position = diff % length
      val quadrant = (position * 4) / length
      var percent = sinApprox(2 * PI * (position.toDouble) / (length.toDouble))
      percent = floor(percent * 1000) / 10
      var description = ""
      if (percent > 95) {
        description = " peak"
      } else {
        if (percent < (-95)) {
          description = " valley"
        } else {
          if (absFloat(percent) < 5) {
            description = " critical transition"
          } else {
            val daysToAdd = (quadrant + 1) * length / 4 - position
            val res = addDays(ty, tm, td, daysToAdd)
            val ny = (res).apply(0)
            val nm = (res).apply(1)
            val nd = (res).apply(2)
            val transition = dateString(ny, nm, nd)
            val trend = ((quadrants).apply(quadrant)).apply(0)
            val next = ((quadrants).apply(quadrant)).apply(1)
            var pct = percent.toString
            if (!contains(pct, ".")) {
              pct += ".0"
            }
            description = " " + pct + "% (" + trend + ", next " + next + " " + transition + ")"
          }
        }
      }
      var posStr = position.toString
      if (position < 10) {
        posStr = " " + posStr
      }
      println(cycle + posStr + " : " + description)
      i += 1
    }
    println("")
  }
  
  def main() = {
    val pairs = List(List("1943-03-09", "1972-07-11"), List("1809-01-12", "1863-11-19"), List("1809-02-12", "1863-11-19"))
    var idx = 0
    while (idx < pairs.length) {
      val p = (pairs).apply(idx)
      biorhythms((p).apply(0), (p).apply(1))
      idx += 1
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
