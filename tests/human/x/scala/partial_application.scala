object partial_application {
  def add(a: Int, b: Int): Int = a + b
  def main(args: Array[String]): Unit = {
    val add5 = add(_: Int, _: Int).curried(5)
    println(add5(3))
  }
}
