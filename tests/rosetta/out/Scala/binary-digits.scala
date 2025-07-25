object binary_digits {
  def toBin(n: Int): String = {
    if (n == 0) {
      return "0"
    }
    var bits = ""
    var x: Int = n
    while (x > 0) {
      bits = x % 2.toString + bits
      x = (x / 2).toInt
    }
    return bits
  }
  
  def main(args: Array[String]): Unit = {
    for(i <- 0 until 16) {
      println(toBin(i))
    }
  }
}
