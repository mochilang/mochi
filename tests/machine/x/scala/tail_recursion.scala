object tail_recursion {
  def sum_rec(n: Int, acc: Int): Int = {
    if ((n).asInstanceOf[Int] == 0) {
      return acc
    }
    return sum_rec((n).asInstanceOf[Int] - 1, (acc).asInstanceOf[Int] + (n).asInstanceOf[Int])
  }
  
  def main(args: Array[String]): Unit = {
    println((sum_rec(10, 0)))
  }
}
