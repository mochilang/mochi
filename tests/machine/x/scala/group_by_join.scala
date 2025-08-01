// Generated by Mochi compiler v0.10.27 on 1970-01-01T00:00:00Z
object group_by_join {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int)
  case class Stat(o: Order, c: Customer)
  case class Stat1(name: String, count: Int)

  case class _Group[K,T](key: K, items: List[T]) extends Iterable[T] { def iterator: Iterator[T] = items.iterator }

  def main(args: Array[String]): Unit = {
    val customers = List(Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"))
    val orders = List(Order(id = 100, customerId = 1), Order(id = 101, customerId = 1), Order(id = 102, customerId = 2))
    val stats = ((for { o <- orders; c <- customers; if o.customerId == (c.id).asInstanceOf[Int] } yield (c.name, Stat(o = o, c = c))).groupBy(_._1).map{ case(k,list) => _Group(k, list.map(_._2)) }.toList).map{ g => Stat1(name = g.key, count = (g).size) }.toList
    println("--- Orders per customer ---")
    for(s <- stats) {
      println(s"${s.name} orders: ${s.count}")
    }
  }
}
