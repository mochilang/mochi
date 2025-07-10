object group_by_join {
  val customers = List[Map[String, Any]](Map[String, Any]("id" -> (1), "name" -> ("Alice")), Map[String, Any]("id" -> (2), "name" -> ("Bob")))
  val orders = List[Map[String, Int]](Map[String, Int]("id" -> (100), "customerId" -> (1)), Map[String, Int]("id" -> (101), "customerId" -> (1)), Map[String, Int]("id" -> (102), "customerId" -> (2)))
  val stats = ((for { o <- orders; c <- customers; if o("customerId") == (c.id).asInstanceOf[Int] } yield (c("name"), (o, c))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).map{ case(gKey,gItems) => { val g = (gKey, gItems); Map[String, Any]("name" -> (g._1), "count" -> ((g._2).size)) } }.toList
  def main(args: Array[String]): Unit = {
    println(("--- Orders per customer ---"))
    for(s <- stats) {
      println((s("name")) + " " + ("orders:") + " " + (s("count")))
    }
  }
}
