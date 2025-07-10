object sort_stable {
  case class Auto1(n: Int, v: String)

  val items = List[Auto1](Auto1(n = 1, v = "a"), Auto1(n = 1, v = "b"), Auto1(n = 2, v = "c"))
  val result = (for { i <- items } yield i.v).sortBy(i => i.n)
  def main(args: Array[String]): Unit = {
    println((result))
  }
}
