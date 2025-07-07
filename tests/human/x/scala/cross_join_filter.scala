object cross_join_filter {
  case class Pair(n: Int, l: String)

  def main(args: Array[String]): Unit = {
    val nums = List(1, 2, 3)
    val letters = List("A", "B")

    val pairs = for {
      n <- nums if n % 2 == 0
      l <- letters
    } yield Pair(n, l)

    println("--- Even pairs ---")
    pairs.foreach(p => println(s"${p.n} ${p.l}"))
  }
}

