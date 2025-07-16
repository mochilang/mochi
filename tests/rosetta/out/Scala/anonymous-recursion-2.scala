object anonymous_recursion_2 {
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
    for(i <- List(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) {
      if (i < 0) {
        println(("fib(" + i.toString).asInstanceOf[Int] + ") returned error: negative n is forbidden")
      } else {
        println((("fib(" + i.toString).asInstanceOf[Int] + ") = ").asInstanceOf[Int] + fib(i).toString)
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
