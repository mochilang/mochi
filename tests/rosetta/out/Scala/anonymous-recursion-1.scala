object anonymous_recursion_1 {
  def fib(n: Int): Int = {
    if (n < 2) {
      return n
    }
    var a = 0
    var b = 1
    var i = 1
    while (i < n) {
      val t = a + b
      a = b
      b = t
      i += 1
    }
    return b
  }
  
  def main() = {
    for(n <- List(0, 1, 2, 3, 4, 5, 10, 40, -1)) {
      if (n < 0) {
        println("fib undefined for negative numbers")
      } else {
        println("fib " + n.toString + " = " + fib(n).toString)
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
