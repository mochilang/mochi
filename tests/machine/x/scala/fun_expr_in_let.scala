object fun_expr_in_let {
  val square = (x: Int) => (x).asInstanceOf[Int] * (x).asInstanceOf[Int]
  def main(args: Array[String]): Unit = {
    println((square(6)))
  }
}
