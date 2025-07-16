object averages_simple_moving_average {
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
  
  def fmt3(x: Double): String = {
    var y = ((x * 1000) + 0.5).toInt.toDouble / 1000
    var s = y.toString
    var dot = indexOf(s, ".")
    if (dot == 0 - 1) {
      s += ".000"
    } else {
      var decs = s.length - dot - 1
      if (decs > 3) {
        s = s.substring(0, dot + 4)
      } else {
        while (decs < 3) {
          s += "0"
          decs += 1
        }
      }
    }
    return s
  }
  
  def pad(s: String, width: Int): String = {
    var out = s
    while (out.length < width) {
      out = " " + out
    }
    return out
  }
  
  def smaSeries(xs: List[Double], period: Int): List[Double] = {
    var res: List[Double] = scala.collection.mutable.ArrayBuffer[Any]()
    var sum = 0
    var i = 0
    while (i < xs.length) {
      sum += (xs).apply(i)
      if (i >= period) {
        sum -= (xs).apply(i - period)
      }
      var denom = i + 1
      if (denom > period) {
        denom = period
      }
      res = res :+ sum / (denom.toDouble)
      i += 1
    }
    return res
  }
  
  def main() = {
    var xs = scala.collection.mutable.ArrayBuffer(1, 2, 3, 4, 5, 5, 4, 3, 2, 1)
    var sma3 = smaSeries(xs, 3)
    var sma5 = smaSeries(xs, 5)
    println("x       sma3   sma5")
    var i = 0
    while (i < xs.length) {
      val line = pad(fmt3((xs).apply(i)), 5) + "  " + pad(fmt3((sma3).apply(i)), 5) + "  " + pad(fmt3((sma5).apply(i)), 5)
      println(line)
      i += 1
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
