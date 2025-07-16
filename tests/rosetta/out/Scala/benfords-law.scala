object benfords_law {
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
  
  def fmtF3(x: Double): String = {
    var y = floorf(x * 1000 + 0.5) / 1000
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
  
  def padFloat3(x: Double, width: Int): String = {
    var s = fmtF3(x)
    while (s.length < width) {
      s = " " + s
    }
    return s
  }
  
  def fib1000(): List[Double] = {
    var a = 0
    var b = 1
    var res: List[Double] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < 1000) {
      res = res :+ b
      var t = b
      b += a
      a = t
      i += 1
    }
    return res
  }
  
  def leadingDigit(x: Double): Int = {
    if (x < 0) {
      x = -x
    }
    while (x >= 10) {
      x /= 10
    }
    while (x > 0 && x < 1) {
      x *= 10
    }
    return x.toInt
  }
  
  def show(nums: List[Double], title: String) = {
    var counts = scala.collection.mutable.ArrayBuffer(0, 0, 0, 0, 0, 0, 0, 0, 0)
    for(n <- nums) {
      val d = leadingDigit(n)
      if (d >= 1 && d <= 9) {
        counts(d - 1) = (counts).apply(d - 1) + 1
      }
    }
    val preds = List(0.301, 0.176, 0.125, 0.097, 0.079, 0.067, 0.058, 0.051, 0.046)
    val total = nums.length
    println(title)
    println("Digit  Observed  Predicted")
    var i = 0
    while (i < 9) {
      val obs = ((counts).apply(i).toDouble) / (total.toDouble)
      var line = "  " + i + 1.toString + "  " + padFloat3(obs, 9) + "  " + padFloat3((preds).apply(i), 8)
      println(line)
      i += 1
    }
  }
  
  def main() = {
    show(fib1000(), "First 1000 Fibonacci numbers")
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
