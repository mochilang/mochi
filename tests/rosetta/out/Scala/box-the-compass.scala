object box_the_compass {
  def padLeft(s: String, w: Int): String = {
    var res = ""
    var n = w - s.length
    while (n > 0) {
      res += " "
      n -= 1
    }
    return res + s
  }
  
  def padRight(s: String, w: Int): String = {
    var out = s
    var i = s.length
    while (i < w) {
      out += " "
      i += 1
    }
    return out
  }
  
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
  
  def format2(f: Double): String = {
    var s = f.toString
    val idx = indexOf(s, ".")
    if (idx < 0) {
      s += ".00"
    } else {
      var need = idx + 3
      if (s.length > need) {
        s = s.substring(0, need)
      } else {
        while (s.length < need) {
          s += "0"
        }
      }
    }
    return s
  }
  
  def cpx(h: Double): Int = {
    var x = ((h / 11.25) + 0.5).toInt
    x %= 32
    if (x < 0) {
      x += 32
    }
    return x
  }
  
  def degrees2compasspoint(h: Double): String = (compassPoint).apply(cpx(h))
  
  def main(args: Array[String]): Unit = {
    val compassPoint = List("North", "North by east", "North-northeast", "Northeast by north", "Northeast", "Northeast by east", "East-northeast", "East by north", "East", "East by south", "East-southeast", "Southeast by east", "Southeast", "Southeast by south", "South-southeast", "South by east", "South", "South by west", "South-southwest", "Southwest by south", "Southwest", "Southwest by west", "West-southwest", "West by south", "West", "West by north", "West-northwest", "Northwest by west", "Northwest", "Northwest by north", "North-northwest", "North by west")
    val headings = List(0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135, 151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38)
    println("Index  Compass point         Degree")
    var i = 0
    while (i < headings.length) {
      val h = (headings).apply(i)
      val idx = i % 32 + 1
      val cp = degrees2compasspoint(h)
      println(padLeft(idx.toString, 4) + "   " + padRight(cp, 19) + " " + format2(h) + "°")
      i += 1
    }
  }
}
