object group_by_sort {
  val items = List[Map[String, Any]](Map[String, Any]("cat" -> ("a"), "val" -> (3)), Map[String, Any]("cat" -> ("a"), "val" -> (1)), Map[String, Any]("cat" -> ("b"), "val" -> (5)), Map[String, Any]("cat" -> ("b"), "val" -> (2)))
  val grouped = (((for { i <- items } yield (i("cat"), (i))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).sortBy(g => -(for { x <- g } yield x.val).sum)).map{ case(gKey,gItems) => { val g = (gKey, gItems); Map[String, Any]("cat" -> (g._1), "total" -> ((for { x <- g._2 } yield x.val).sum)) } }.toList
  def main(args: Array[String]): Unit = {
    println((grouped))
  }
}
