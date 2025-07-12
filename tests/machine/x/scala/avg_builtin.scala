object avg_builtin {
  def _safe_div(a: Double, b: Double): Double = if(b == 0) 0 else a / b

  def main(args: Array[String]): Unit = {
    println((_safe_div((List[Int](1, 2, 3)).sum.toDouble, (List[Int](1, 2, 3)).size)))
  }
}
