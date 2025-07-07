object cross_join_triple {
  case class Combo(n: Int, l: String, b: Boolean)

  def main(args: Array[String]): Unit = {
    val nums = List(1, 2)
    val letters = List("A", "B")
    val bools = List(true, false)

    val combos = for {
      n <- nums
      l <- letters
      b <- bools
    } yield Combo(n, l, b)

    println("--- Cross Join of three lists ---")
    combos.foreach(c => println(s"${c.n} ${c.l} ${c.b}"))
  }
}

