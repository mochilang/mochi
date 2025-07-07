object group_items_iteration {
  case class Data(tag: String, value: Int)
  case class Result(tag: String, total: Int)

  def main(args: Array[String]): Unit = {
    val data = List(Data("a",1), Data("a",2), Data("b",3))
    val groups = data.groupBy(_.tag)

    val tmp = groups.map { case (tag, items) =>
      val total = items.map(_.value).sum
      Result(tag, total)
    }.toList.sortBy(_.tag)

    tmp.foreach(r => println(s"{tag: ${r.tag}, total: ${r.total}}"))
  }
}
