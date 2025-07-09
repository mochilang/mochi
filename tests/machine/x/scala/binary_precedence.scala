object binary_precedence {
  def main(args: Array[String]): Unit = {
    println(((1 + 2).asInstanceOf[Int] * 3))
    println(((1 + 2) * 3))
    println(((2 * 3).asInstanceOf[Int] + 1))
    println((2 * (3 + 1)))
  }
}
