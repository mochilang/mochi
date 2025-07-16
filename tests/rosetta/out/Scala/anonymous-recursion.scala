object anonymous_recursion {
  def fib(n: Int): Int = {
    if (n < 2) {
      return n
    }
    return fib(n - 1) + fib(n - 2)
  }
  
  def main() = {
    var i = -1
    while (i <= 10) {
      if (i < 0) {
        println(("fib(" + i.toString).asInstanceOf[Int] + ") returned error: negative n is forbidden")
      } else {
        println((("fib(" + i.toString).asInstanceOf[Int] + ") = ").asInstanceOf[Int] + fib(i).toString)
      }
      i += 1
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
