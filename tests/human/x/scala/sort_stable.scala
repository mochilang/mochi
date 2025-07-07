object sort_stable {
  case class Item(n: Int, v: String)

  def main(args: Array[String]): Unit = {
    val items = List(Item(1, "a"), Item(1, "b"), Item(2, "c"))
    val result = items.sortBy(_.n).map(_.v)
    println(result)
  }
}

