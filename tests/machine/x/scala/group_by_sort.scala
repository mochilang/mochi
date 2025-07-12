object group_by_sort {
  case class Grouped(i: Item)
  case class Grouped1(cat: String, total: Int)
  case class Item(cat: String, `val`: Int)

  case class _Group[K,T](key: K, items: List[T])

  val items = List[Item](Item(cat = "a", `val` = 3), Item(cat = "a", `val` = 1), Item(cat = "b", `val` = 5), Item(cat = "b", `val` = 2))
  val grouped = (((for { i <- items } yield (i.cat, Grouped(i = i))).groupBy(_._1).map{ case(k,list) => _Group(k, list.map(_._2)) }.toList).sortBy(g => -(for { x <- g } yield x.`val`).sum)).map{ g => { val g = g; Grouped1(cat = g.key, total = (for { x <- g } yield x.`val`).sum) } }.toList
  def main(args: Array[String]): Unit = {
    println((grouped))
  }
}
