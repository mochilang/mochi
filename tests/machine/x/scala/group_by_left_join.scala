object group_by_left_join {
  case class Auto1(id: Int, name: String)
  case class Auto2(id: Int, customerId: Int)
  case class Auto3(name: Any, count: Int)

  val customers = List[Auto1](Auto1(id = 1, name = "Alice"), Auto1(id = 2, name = "Bob"), Auto1(id = 3, name = "Charlie"))
  val orders = List[Auto2](Auto2(id = 100, customerId = 1), Auto2(id = 101, customerId = 1), Auto2(id = 102, customerId = 2))
  val stats = ((for { c <- customers; o = orders.find(o => (o.customerId).asInstanceOf[Int] == c.id) } yield (c.name, (c, o))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).map{ case(gKey,gItems) => { val g = (gKey, gItems); Auto3(name = g._1, count = (for { r <- g._2; if r.o } yield r).size) } }.toList
  def main(args: Array[String]): Unit = {
    println(("--- Group Left Join ---"))
    for(s <- stats) {
      println((s.name) + " " + ("orders:") + " " + (s.count))
    }
  }
}
