object group_by_multi_join {
  val nations = List[Map[String, Any]](Map[String, Any]("id" -> (1), "name" -> ("A")), Map[String, Any]("id" -> (2), "name" -> ("B")))
  val suppliers = List[Map[String, Int]](Map[String, Int]("id" -> (1), "nation" -> (1)), Map[String, Int]("id" -> (2), "nation" -> (2)))
  val partsupp = List[Map[String, Any]](Map[String, Any]("part" -> (100), "supplier" -> (1), "cost" -> (10), "qty" -> (2)), Map[String, Any]("part" -> (100), "supplier" -> (2), "cost" -> (20), "qty" -> (1)), Map[String, Any]("part" -> (200), "supplier" -> (1), "cost" -> (5), "qty" -> (3)))
  val filtered = for { ps <- partsupp; s <- suppliers; if (s.id).asInstanceOf[Int] == ps("supplier"); n <- nations; if (n.id).asInstanceOf[Int] == s("nation"); if n("name") == "A" } yield Map[String, Any]("part" -> (ps("part")), "value" -> (ps("cost") * ps("qty")))
  val grouped = ((for { x <- filtered } yield (x("part"), (x))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).map{ case(gKey,gItems) => { val g = (gKey, gItems); Map[String, Any]("part" -> (g._1), "total" -> ((for { r <- g._2 } yield r.value).sum)) } }.toList
  def main(args: Array[String]): Unit = {
    println((grouped))
  }
}
