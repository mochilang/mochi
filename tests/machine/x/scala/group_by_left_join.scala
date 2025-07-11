object group_by_left_join {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int)
  case class Stat(name: String, count: Int)

  val customers = List[Customer](Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"), Customer(id = 3, name = "Charlie"))
  val orders = List[Order](Order(id = 100, customerId = 1), Order(id = 101, customerId = 1), Order(id = 102, customerId = 2))
  val stats = ((for { c <- customers; o = orders.find(o => (o.customerId).asInstanceOf[Int] == c.id) } yield (c.name, (c, o))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).map{ case(gKey,gItems) => { val g = (gKey, gItems); Stat(name = g._1, count = (for { r <- g._2; if r.o } yield r).size) } }.toList
  def main(args: Array[String]): Unit = {
    println(("--- Group Left Join ---"))
    for(s <- stats) {
      println((s.name) + " " + ("orders:") + " " + (s.count))
    }
  }
}
