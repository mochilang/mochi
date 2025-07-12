object if_then_else_nested {
  val x = 8
  val msg = if (x > 10) "big" else if (x > 5) "medium" else "small"
  def main(args: Array[String]): Unit = {
    println(msg)
  }
}
