object fun_expr_in_let {
  def main(args: Array[String]): Unit = {
    val square = (x: Int) => (x).asInstanceOf[Int] * (x).asInstanceOf[Int]
    println((square(6)))
  }
}
