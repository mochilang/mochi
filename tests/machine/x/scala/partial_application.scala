object partial_application {
  def add(a: Int, b: Int): Int = {
    return a + b
  }
  
  def main(args: Array[String]): Unit = {
    val add5 = add(5)
    println(add5(3))
  }
}
