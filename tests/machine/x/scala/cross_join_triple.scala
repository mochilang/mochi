object cross_join_triple {
  case class Auto1(n: Int, l: String, b: Boolean)
  case class Auto2(n: Any, l: Any, b: Any)

  val nums = List[Int](1, 2)
  val letters = List[String]("A", "B")
  val bools = List[Boolean](true, false)
  val combos = for { n <- nums; l <- letters; b <- bools } yield Auto1(n = n, l = l, b = b)
  def main(args: Array[String]): Unit = {
    println(("--- Cross Join of three lists ---"))
    for(c <- combos) {
      println((c.n) + " " + (c.l) + " " + (c.b))
    }
  }
}
