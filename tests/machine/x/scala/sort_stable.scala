object sort_stable {
  case class Item(n: Int, v: String)

  val items = List[Item](Item(n = 1, v = "a"), Item(n = 1, v = "b"), Item(n = 2, v = "c"))
  val result = (for { i <- items } yield i).sortBy(i => i.n).map(i => i.v)
  def main(args: Array[String]): Unit = {
    println((result))
  }
}
