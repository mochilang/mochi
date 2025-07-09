object fun_call {
  def add(a: Int, b: Int): Int = {
    return (a).asInstanceOf[Int] + (b).asInstanceOf[Int]
  }
  
  def main(args: Array[String]): Unit = {
    println((add(2, 3)))
  }
}
