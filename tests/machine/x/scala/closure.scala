object closure {
  def makeAdder(n: Int): (Int) => Int = {
    return (x: Int) => (x).asInstanceOf[Int] + (n).asInstanceOf[Int]
  }
  
  def main(args: Array[String]): Unit = {
    val add10 = makeAdder(10)
    println((add10(7)))
  }
}
