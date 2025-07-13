object cross_join_filter {
  case class Pair(n: Int, l: String)

  val nums = List(1, 2, 3)
  val letters = List("A", "B")
  val pairs = for { n <- nums; l <- letters; if n % 2 == 0 } yield Pair(n = n, l = l)
  def main(args: Array[String]): Unit = {
    println("--- Even pairs ---")
    for(p <- pairs) {
      println(s"${p.n} ${p.l}")
    }
  }
}
