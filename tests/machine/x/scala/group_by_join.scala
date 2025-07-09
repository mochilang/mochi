object group_by_join {
  def main(args: Array[String]): Unit = {
    val customers = List(Map("id" -> (1), "name" -> ("Alice")), Map("id" -> (2), "name" -> ("Bob")))
    val orders = List(Map("id" -> (100), "customerId" -> (1)), Map("id" -> (101), "customerId" -> (1)), Map("id" -> (102), "customerId" -> (2)))
    val stats = ((for { o <- orders; c <- customers; if (o.customerId).asInstanceOf[Int] == (c.id).asInstanceOf[Int] } yield (c.name, (o, c))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).map{ case(gKey,gItems) => { val g = (gKey, gItems); Map("name" -> (g._1), "count" -> ((g)._2.size)) } }.toList
    println(("--- Orders per customer ---"))
    for(s <- stats) {
      println((s("name")) + " " + ("orders:") + " " + (s("count")))
    }
  }
}
