object math_ops {
  def _safe_div(a: Double, b: Double): Double = if(b == 0) 0 else a / b

  def main(args: Array[String]): Unit = {
    println((6 * 7))
    println((_safe_div(7, 2)))
    println((7 % 2))
  }
}
