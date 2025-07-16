object bitwise_operations {
  def toUnsigned16(n: Int): Int = {
    var u = n
    if (u < 0) {
      u += 65536
    }
    return u % 65536
  }
  
  def bin16(n: Int): String = {
    var u = toUnsigned16(n)
    var bits = ""
    var mask = 32768
    for(i <- 0 until 16) {
      if (u >= mask) {
        bits += "1"
        u -= mask
      } else {
        bits += "0"
      }
      mask = (mask / 2).toInt
    }
    return bits
  }
  
  def bit_and(a: Int, b: Int): Int = {
    var ua = toUnsigned16(a)
    var ub = toUnsigned16(b)
    var res = 0
    var bit = 1
    for(i <- 0 until 16) {
      if ((ua % 2 == 1 && ub % 2).asInstanceOf[Int] == 1) {
        res += bit
      }
      ua = (ua / 2).toInt
      ub = (ub / 2).toInt
      bit *= 2
    }
    return res
  }
  
  def bit_or(a: Int, b: Int): Int = {
    var ua = toUnsigned16(a)
    var ub = toUnsigned16(b)
    var res = 0
    var bit = 1
    for(i <- 0 until 16) {
      if ((ua % 2 == 1 || ub % 2).asInstanceOf[Int] == 1) {
        res += bit
      }
      ua = (ua / 2).toInt
      ub = (ub / 2).toInt
      bit *= 2
    }
    return res
  }
  
  def bit_xor(a: Int, b: Int): Int = {
    var ua = toUnsigned16(a)
    var ub = toUnsigned16(b)
    var res = 0
    var bit = 1
    for(i <- 0 until 16) {
      val abit = ua % 2
      val bbit = ub % 2
      if ((abit == 1 && bbit == 0) || (abit == 0 && bbit == 1)) {
        res += bit
      }
      ua = (ua / 2).toInt
      ub = (ub / 2).toInt
      bit *= 2
    }
    return res
  }
  
  def bit_not(a: Int): Int = {
    var ua = toUnsigned16(a)
    return 65535 - ua
  }
  
  def shl(a: Int, b: Int): Int = {
    var ua = toUnsigned16(a)
    var i = 0
    while (i < b) {
      ua = (ua * 2) % 65536
      i += 1
    }
    return ua
  }
  
  def shr(a: Int, b: Int): Int = {
    var ua = toUnsigned16(a)
    var i = 0
    while (i < b) {
      ua = (ua / 2).toInt
      i += 1
    }
    return ua
  }
  
  def las(a: Int, b: Int): Int = shl(a, b)
  
  def ras(a: Int, b: Int): Int = {
    var val = a
    var i = 0
    while (i < b) {
      if (val >= 0) {
        val = (val / 2).toInt
      } else {
        val = ((val - 1) / 2).toInt
      }
      i += 1
    }
    return toUnsigned16(val)
  }
  
  def rol(a: Int, b: Int): Int = {
    var ua = toUnsigned16(a)
    val left = shl(ua, b)
    val right = shr(ua, 16 - b)
    return toUnsigned16(left + right)
  }
  
  def ror(a: Int, b: Int): Int = {
    var ua = toUnsigned16(a)
    val right = shr(ua, b)
    val left = shl(ua, 16 - b)
    return toUnsigned16(left + right)
  }
  
  def bitwise(a: Int, b: Int) = {
    println("a:   " + bin16(a))
    println("b:   " + bin16(b))
    println("and: " + bin16(bit_and(a, b)))
    println("or:  " + bin16(bit_or(a, b)))
    println("xor: " + bin16(bit_xor(a, b)))
    println("not: " + bin16(bit_not(a)))
    if (b < 0) {
      println("Right operand is negative, but all shifts require an unsigned right operand (shift distance).")
      return null
    }
    println("shl: " + bin16(shl(a, b)))
    println("shr: " + bin16(shr(a, b)))
    println("las: " + bin16(las(a, b)))
    println("ras: " + bin16(ras(a, b)))
    println("rol: " + bin16(rol(a, b)))
    println("ror: " + bin16(ror(a, b)))
  }
  
  def main(args: Array[String]): Unit = {
    bitwise(-460, 6)
  }
}
