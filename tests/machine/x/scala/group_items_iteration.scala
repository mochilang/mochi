object group_items_iteration {
  case class Auto1(tag: String, total: Int)
  case class Data(tag: String, `val`: Int)
  case class Group(d: Data)

  val data = List[Data](Data(tag = "a", `val` = 1), Data(tag = "a", `val` = 2), Data(tag = "b", `val` = 3))
  val groups = ((for { d <- data } yield (d.tag, Group(d = d))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).map{ case(gKey,gItems) => { val g = (gKey, gItems); g._2 } }.toList
  def main(args: Array[String]): Unit = {
    var tmp = scala.collection.mutable.ArrayBuffer[Any]()
    for(g <- groups) {
      var total = 0
      for(x <- g.items) {
        total += x.`val`
      }
      tmp = tmp :+ Auto1(tag = g.key, total = total)
    }
    val result = (for { r <- tmp } yield r).sortBy(r => r.tag).map(r => r)
    println((result))
  }
}
