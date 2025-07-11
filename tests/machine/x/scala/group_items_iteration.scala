object group_items_iteration {
  case class Auto1(tag: String, `val`: Int)
  case class Auto2(tag: String, total: Int)

  val data = List[Auto1](Auto1(tag = "a", `val` = 1), Auto1(tag = "a", `val` = 2), Auto1(tag = "b", `val` = 3))
  val groups = ((for { d <- data } yield (d.tag, (d))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).map{ case(gKey,gItems) => { val g = (gKey, gItems); g._2 } }.toList
  val result = (for { r <- tmp } yield r).sortBy(r => r.tag)
  def main(args: Array[String]): Unit = {
    var tmp = scala.collection.mutable.ArrayBuffer[Any]()
    for(g <- groups) {
      var total = 0
      for(x <- g._2) {
        total += x.`val`
      }
      tmp = tmp :+ Auto2(tag = g._1, total = total)
    }
    println((result))
  }
}
