object group_by_conditional_sum {
  case class Item(cat: String, value: Int, flag: Boolean)
  case class Result(cat: String, share: Double)

  def main(args: Array[String]): Unit = {
    val items = List(
      Item("a", 10, true),
      Item("a", 5, false),
      Item("b", 20, true)
    )

    val result = items.groupBy(_.cat).toList.sortBy(_._1).map { case (cat, list) =>
      val share = list.map(i => if (i.flag) i.value else 0).sum.toDouble /
        list.map(_.value).sum.toDouble
      Result(cat, share)
    }

    result.foreach(r => println(s"{cat: ${r.cat}, share: ${r.share}}"))
  }
}
