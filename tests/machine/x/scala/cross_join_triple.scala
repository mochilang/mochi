object cross_join_triple {
  def main(args: Array[String]): Unit = {
    val nums = List(1, 2)
    val letters = List("A", "B")
    val bools = List(true, false)
    val combos = for { n <- nums; l <- letters; b <- bools } yield Map("n" -> n, "l" -> l, "b" -> b)
    println("--- Cross Join of three lists ---")
    for(c <- combos) {
      println(c("n") + " " + c("l") + " " + c("b"))
    }
  }
}
