object cross_join_filter {
  case class Auto1(n: Int, l: String)
  case class Auto2(n: Any, l: Any)

  val nums = List[Int](1, 2, 3)
  val letters = List[String]("A", "B")
  val pairs = for { n <- nums; l <- letters; if (n % 2).asInstanceOf[Int] == 0 } yield Auto1(n = n, l = l)
  def main(args: Array[String]): Unit = {
    println(("--- Even pairs ---"))
    for(p <- pairs) {
      println((p.n) + " " + (p.l))
    }
  }
}
