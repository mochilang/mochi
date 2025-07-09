object cross_join_filter {
  val nums = List(1, 2, 3)
  val letters = List("A", "B")
  val pairs = for { n <- nums; l <- letters; if (n % 2).asInstanceOf[Int] == 0 } yield Map("n" -> (n), "l" -> (l))
  def main(args: Array[String]): Unit = {
    println(("--- Even pairs ---"))
    for(p <- pairs) {
      println((p("n")) + " " + (p("l")))
    }
  }
}
