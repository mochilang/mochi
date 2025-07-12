object group_by_left_join {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int)
  case class Stat(c: Customer, o: Option[Order])
  case class Stat1(name: String, count: Int)

  case class _Group[K,T](key: K, items: List[T]) extends Iterable[T] { def iterator: Iterator[T] = items.iterator }

  val customers = List(Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"), Customer(id = 3, name = "Charlie"))
  val orders = List(Order(id = 100, customerId = 1), Order(id = 101, customerId = 1), Order(id = 102, customerId = 2))
  val stats = ((for { c <- customers; o = orders.find(o => (o.customerId).asInstanceOf[Int] == c.id) } yield (c.name, Stat(c = c, o = o))).groupBy(_._1).map{ case(k,list) => _Group(k, list.map(_._2)) }.toList).map{ g => Stat1(name = g.key, count = (for { r <- g; if r.o.nonEmpty } yield r).size) }.toList
  def main(args: Array[String]): Unit = {
    println("--- Group Left Join ---")
    for(s <- stats) {
      println(s.name + " " + "orders:" + " " + s.count)
    }
  }
}
