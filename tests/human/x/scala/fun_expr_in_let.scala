object fun_expr_in_let {
  def main(args: Array[String]): Unit = {
    val square: Int => Int = x => x * x
    println(square(6))
  }
}
