object tail_recursion {
  def sum_rec(n: Int, acc: Int): Int = {
    if (n == 0) {
      return acc
    }
    return sum_rec(n - 1, acc + n)
  }
  
  def main(args: Array[String]): Unit = {
    println((sum_rec(10, 0)))
  }
}
