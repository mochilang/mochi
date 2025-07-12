object group_by_join {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int)
  case class Stat(o: Order, c: Customer)
  case class Stat1(name: String, count: Int)

  val customers = List[Customer](Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"))
  val orders = List[Order](Order(id = 100, customerId = 1), Order(id = 101, customerId = 1), Order(id = 102, customerId = 2))
  val stats = ((for { o <- orders; c <- customers; if o.customerId == (c.id).asInstanceOf[Int] } yield (c.name, Stat(o = o, c = c))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).map{ case(gKey,gItems) => { val g = (gKey, gItems); Stat1(name = g.key, count = (g._2).size) } }.toList
  def main(args: Array[String]): Unit = {
    println(("--- Orders per customer ---"))
    for(s <- stats) {
      println((s.name) + " " + ("orders:") + " " + (s.count))
    }
  }
}
