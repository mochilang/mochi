object call_a_function_12 {
  def mkAdd(a: Int): (Int) => Int = (b: Int) => a + b
  
  def mysum(x: Int, y: Int): Int = x + y
  
  def partialSum(x: Int): (Int) => Int = (y: Int) => mysum(x, y)
  
  def main() = {
    val add2 = mkAdd(2)
    val add3 = mkAdd(3)
    println(add2(5).toString + " " + add3(6).toString)
    val partial = partialSum(13)
    println(partial(5).toString)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
