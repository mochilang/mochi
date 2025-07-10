object group_by_sort {
  case class Auto1(cat: String, val: Int)
  case class Auto2(cat: Any, total: Int)

  val items = List[Auto1](Auto1(cat = "a", val = 3), Auto1(cat = "a", val = 1), Auto1(cat = "b", val = 5), Auto1(cat = "b", val = 2))
  val grouped = (((for { i <- items } yield (i.cat, (i))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).sortBy(g => -(for { x <- g } yield x.val).sum)).map{ case(gKey,gItems) => { val g = (gKey, gItems); Auto2(cat = g._1, total = (for { x <- g._2 } yield x.val).sum) } }.toList
  def main(args: Array[String]): Unit = {
    println((grouped))
  }
}
