object babbage_problem {
  def main(args: Array[String]): Unit = {
    val target = 269696
    val modulus = 1000000
    var n = 1
    while (true) {
      val square = n * n
      val ending = square % modulus
      if (ending == target) {
        println("The smallest number whose square ends with " + target.toString + " is " + n.toString)
        return
      }
      n += 1
    }
  }
}
