object partial_application {
  def add(a: Int, b: Int): Int = a + b
  
  def main(args: Array[String]): Unit = {
    val add5 = (p0: Int) => add(5, p0)
    println(add5(3))
  }
}
