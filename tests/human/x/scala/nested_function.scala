object nested_function {
  def outer(x: Int): Int = {
    def inner(y: Int): Int = x + y
    inner(5)
  }
  def main(args: Array[String]): Unit = {
    println(outer(3))
  }
}
