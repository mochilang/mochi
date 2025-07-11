object group_by_join {
  case class Auto1(id: Int, name: String)
  case class Auto2(id: Int, customerId: Int)
  case class Auto3(name: String, count: Int)

  val customers = List[Auto1](Auto1(id = 1, name = "Alice"), Auto1(id = 2, name = "Bob"))
  val orders = List[Auto2](Auto2(id = 100, customerId = 1), Auto2(id = 101, customerId = 1), Auto2(id = 102, customerId = 2))
  val stats = ((for { o <- orders; c <- customers; if o.customerId == (c.id).asInstanceOf[Int] } yield (c.name, (o, c))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).map{ case(gKey,gItems) => { val g = (gKey, gItems); Auto3(name = g._1, count = (g._2).size) } }.toList
  def main(args: Array[String]): Unit = {
    println(("--- Orders per customer ---"))
    for(s <- stats) {
      println((s.name) + " " + ("orders:") + " " + (s.count))
    }
  }
}
