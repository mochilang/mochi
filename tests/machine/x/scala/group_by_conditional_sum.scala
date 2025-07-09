object group_by_conditional_sum {
  val items = List(Map("cat" -> ("a"), "val" -> (10), "flag" -> (true)), Map("cat" -> ("a"), "val" -> (5), "flag" -> (false)), Map("cat" -> ("b"), "val" -> (20), "flag" -> (true)))
  val result = (((for { i <- items } yield (i("cat"), (i))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).sortBy(g => g.key)).map{ case(gKey,gItems) => { val g = (gKey, gItems); Map("cat" -> (g._1), "share" -> (for { x <- g } yield if ((x.flag).asInstanceOf[Boolean]) x.val else 0.sum / for { x <- g } yield x.val.sum)) } }.toList
  def main(args: Array[String]): Unit = {
    println((result))
  }
}
