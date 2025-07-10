object cross_join_triple {
  val nums = List[Int](1, 2)
  val letters = List[String]("A", "B")
  val bools = List[Boolean](true, false)
  val combos = for { n <- nums; l <- letters; b <- bools } yield Map[String, Any]("n" -> (n), "l" -> (l), "b" -> (b))
  def main(args: Array[String]): Unit = {
    println(("--- Cross Join of three lists ---"))
    for(c <- combos) {
      println((c("n")) + " " + (c("l")) + " " + (c("b")))
    }
  }
}
