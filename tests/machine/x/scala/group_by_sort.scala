object group_by_sort {
  case class Grouped(cat: String, total: Int)
  case class Item(cat: String, `val`: Int)

  val items = List[Item](Item(cat = "a", `val` = 3), Item(cat = "a", `val` = 1), Item(cat = "b", `val` = 5), Item(cat = "b", `val` = 2))
  val grouped = (((for { i <- items } yield (i.cat, (i))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).sortBy(g => -(for { x <- g._2 } yield x.`val`).sum)).map{ case(gKey,gItems) => { val g = (gKey, gItems); Grouped(cat = g._1, total = (for { x <- g._2 } yield x.`val`).sum) } }.toList
  def main(args: Array[String]): Unit = {
    println((grouped))
  }
}
