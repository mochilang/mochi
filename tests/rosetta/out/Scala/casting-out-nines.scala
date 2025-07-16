object casting_out_nines {
  case class TestCase(base: Int, begin: String, end: String, kaprekar: List[String])

  def parseIntBase(s: String, base: Int): Int = {
    val digits = "0123456789abcdefghijklmnopqrstuvwxyz"
    var n = 0
    var i = 0
    while (i < s.length) {
      var j = 0
      var v = 0
      while (j < digits.length) {
        if (digits.substring(j, j + 1) == s.substring(i, i + 1)) {
          v = j
          return
        }
        j += 1
      }
      n = n * base + v
      i += 1
    }
    return n
  }
  
  def intToBase(n: Int, base: Int): String = {
    val digits = "0123456789abcdefghijklmnopqrstuvwxyz"
    if (n == 0) {
      return "0"
    }
    var out = ""
    var v = n
    while (v > 0) {
      val d = v % base
      out = digits.substring(d, d + 1) + out
      v /= base
    }
    return out
  }
  
  def subset(base: Int, begin: String, end: String): List[String] = {
    var b = parseIntBase(begin, base)
    var e = parseIntBase(end, base)
    var out: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var k = b
    while (k <= e) {
      val ks = intToBase(k, base)
      val mod = base - 1
      val r1 = parseIntBase(ks, base) % mod
      val r2 = (parseIntBase(ks, base) * parseIntBase(ks, base)) % mod
      if (r1 == r2) {
        out = out :+ ks
      }
      k += 1
    }
    return out
  }
  
  def main(args: Array[String]): Unit = {
    val testCases = List(Map("base" -> 10, "begin" -> "1", "end" -> "100", "kaprekar" -> List("1", "9", "45", "55", "99")), Map("base" -> 17, "begin" -> "10", "end" -> "gg", "kaprekar" -> List("3d", "d4", "gg")))
    var idx = 0
    while (idx < testCases.length) {
      val tc = (testCases).apply(idx)
      println(((((("\nTest case base = " + (tc).apply("base").toString).asInstanceOf[Int] + ", begin = ").asInstanceOf[Int] + ((tc).apply("begin")).asInstanceOf[Int]).asInstanceOf[Int] + ", end = ").asInstanceOf[Int] + ((tc).apply("end")).asInstanceOf[Int]).asInstanceOf[Int] + ":")
      val s = subset((tc).apply("base"), (tc).apply("begin"), (tc).apply("end"))
      println("Subset:  " + s.toString)
      println("Kaprekar:" + (tc).apply("kaprekar").toString)
      var sx = 0
      var valid = true
      var i = 0
      while (i < (tc).apply("kaprekar").length) {
        val k = ((tc).apply("kaprekar")).apply(i)
        var found = false
        while (sx < s.length) {
          if ((s).apply(sx) == (k).asInstanceOf[Int]) {
            found = true
            sx += 1
            return
          }
          sx += 1
        }
        if (!found) {
          println(("Fail:" + (k).asInstanceOf[Int]).asInstanceOf[Int] + " not in subset")
          valid = false
          return
        }
        i += 1
      }
      if (valid) {
        println("Valid subset.")
      }
      idx += 1
    }
  }
}
