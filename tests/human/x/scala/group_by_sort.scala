object group_by_sort {
  case class Item(cat: String, value: Int)
  case class Result(cat: String, total: Int)

  def main(args: Array[String]): Unit = {
    val items = List(
      Item("a", 3), Item("a", 1),
      Item("b", 5), Item("b", 2)
    )

    val grouped = items.groupBy(_.cat).map { case (cat, list) =>
      Result(cat, list.map(_.value).sum)
    }.toList.sortBy(r => -r.total)

    grouped.foreach(r => println(s"{cat: ${r.cat}, total: ${r.total}}"))
  }
}
