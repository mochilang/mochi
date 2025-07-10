object group_by_left_join {
  val customers = List[Map[String, Any]](Map[String, Any]("id" -> (1), "name" -> ("Alice")), Map[String, Any]("id" -> (2), "name" -> ("Bob")), Map[String, Any]("id" -> (3), "name" -> ("Charlie")))
  val orders = List[Map[String, Int]](Map[String, Int]("id" -> (100), "customerId" -> (1)), Map[String, Int]("id" -> (101), "customerId" -> (1)), Map[String, Int]("id" -> (102), "customerId" -> (2)))
  val stats = ((for { c <- customers; o = orders.find(o => (o.customerId).asInstanceOf[Int] == c("id")) } yield (c("name"), (c, o))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).map{ case(gKey,gItems) => { val g = (gKey, gItems); Map[String, Any]("name" -> (g._1), "count" -> ((for { r <- g._2; if r.o } yield r).size)) } }.toList
  def main(args: Array[String]): Unit = {
    println(("--- Group Left Join ---"))
    for(s <- stats) {
      println((s("name")) + " " + ("orders:") + " " + (s("count")))
    }
  }
}
