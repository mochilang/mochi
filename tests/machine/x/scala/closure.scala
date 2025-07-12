object closure {
  def makeAdder(n: Int): (Int) => Int = (x: Int) => x + n
  
  def main(args: Array[String]): Unit = {
    val add10 = makeAdder(10)
    println(add10(7))
  }
}
