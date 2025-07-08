object cross_join_filter {
  def main(args: Array[String]): Unit = {
    val nums = List(1, 2, 3)
    val letters = List("A", "B")
    val pairs = for { n <- nums; l <- letters; if n % 2 == 0 } yield Map("n" -> n, "l" -> l)
    println("--- Even pairs ---")
    for(p <- pairs) {
      println(p("n") + " " + p("l"))
    }
  }
}
