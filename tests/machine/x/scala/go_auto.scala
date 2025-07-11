object go_auto {
  object testpkg {
    def Add(a: Int, b: Int): Int = a + b
    val Pi: Double = 3.14
    val Answer: Int = 42
  }

  def main(args: Array[String]): Unit = {
    println((testpkg.Add(2, 3)))
    println((testpkg.Pi))
    println((testpkg.Answer))
  }
}
