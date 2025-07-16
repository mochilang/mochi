case class Complex(var re: Double, var im: Double)

object arithmetic_complex {
  def add(a: Complex, b: Complex): Complex = Complex(re = a.re + b.re, im = a.im + b.im)
  
  def mul(a: Complex, b: Complex): Complex = Complex(re = a.re * b.re - a.im * b.im, im = a.re * b.im + a.im * b.re)
  
  def neg(a: Complex): Complex = Complex(re = -a.re, im = -a.im)
  
  def inv(a: Complex): Complex = {
    val denom = a.re * a.re + a.im * a.im
    return Complex(re = a.re / denom, im = -a.im / denom)
  }
  
  def conj(a: Complex): Complex = Complex(re = a.re, im = -a.im)
  
  def cstr(a: Complex): String = {
    var s = "(" + a.re.toString
    if (a.im >= 0) {
      s = s + "+" + a.im.toString + "i)"
    } else {
      s = s + a.im.toString + "i)"
    }
    return s
  }
  
  def main(args: Array[String]): Unit = {
    val a = Complex(re = 1, im = 1)
    val b = Complex(re = 3.14159, im = 1.25)
    println("a:       " + cstr(a))
    println("b:       " + cstr(b))
    println("a + b:   " + cstr(add(a, b)))
    println("a * b:   " + cstr(mul(a, b)))
    println("-a:      " + cstr(neg(a)))
    println("1 / a:   " + cstr(inv(a)))
    println("a̅:       " + cstr(conj(a)))
  }
}
