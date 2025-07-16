object chinese_remainder_theorem {
  def egcd(a: Int, b: Int): List[Int] = {
    if (a == 0) {
      return List(b, 0, 1)
    }
    val res = egcd(b % a, a)
    val g = (res).apply(0)
    val x1 = (res).apply(1)
    val y1 = (res).apply(2)
    return List(g, y1 - (b / a) * x1, x1)
  }
  
  def modInv(a: Int, m: Int): Int = {
    val r = egcd(a, m)
    if ((r).apply(0) != 1) {
      return 0
    }
    val x = (r).apply(1)
    if (x < 0) {
      return x + m
    }
    return x
  }
  
  def crt(a: List[Int], n: List[Int]): Int = {
    var prod = 1
    var i = 0
    while (i < n.length) {
      prod *= (n).apply(i)
      i += 1
    }
    var x = 0
    i = 0
    while (i < n.length) {
      val ni = (n).apply(i)
      val ai = (a).apply(i)
      val p = prod / ni
      val inv = modInv(p % ni, ni)
      x = x + ai * inv * p
      i += 1
    }
    return x % prod
  }
  
  def main(args: Array[String]): Unit = {
    val n = List(3, 5, 7)
    val a = List(2, 3, 2)
    val res = crt(a, n)
    println(res.toString + " <nil>")
  }
}
