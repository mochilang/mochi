object arbitrary_precision_integers_included_ {
  def pow_int(base: Int, exp: Int): Int = {
    var result = 1
    var b = base
    var e = exp
    while (e > 0) {
      if (e % 2 == 1) {
        result *= b
      }
      b *= b
      e = (e / 2).toInt
    }
    return result
  }
  
  def pow_big(base: bigint, exp: Int): bigint = {
    var result: bigint = 1
    var b: bigint = base
    var e = exp
    while (e > 0) {
      if (e % 2 == 1) {
        result *= b
      }
      b *= b
      e = (e / 2).toInt
    }
    return result
  }
  
  def main(args: Array[String]): Unit = {
    var e1 = pow_int(3, 2)
    var e2 = pow_int(4, e1)
    var base: bigint = 5
    var x: bigint = pow_big(base, e2)
    var s = x.toString
    println(s"5^(4^(3^2)) has ${s.length} digits: ${s.substring(0, 20)}... ${s.substring(s.length - 20, s.length)}")
  }
}
