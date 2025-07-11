object cross_join_filter {
  case class Pair(n: Int, l: String)

  val nums = List[Int](1, 2, 3)
  val letters = List[String]("A", "B")
  val pairs = for { n <- nums; l <- letters; if (n % 2).asInstanceOf[Int] == 0 } yield Pair(n = n, l = l)
  def main(args: Array[String]): Unit = {
    println(("--- Even pairs ---"))
    for(p <- pairs) {
      println((p.n) + " " + (p.l))
    }
  }
}
