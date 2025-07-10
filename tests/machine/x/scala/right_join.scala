object right_join {
  case class Auto1(id: Int, name: String)
  case class Auto2(id: Int, customerId: Int, total: Int)
  case class Auto3(customerName: String, order: Auto2)

  def _truthy(v: Any): Boolean = v match {
    case null => false
    case b: Boolean => b
    case i: Int => i != 0
    case l: Long => l != 0L
    case d: Double => d != 0.0
    case s: String => s.nonEmpty
    case m: scala.collection.Map[_, _] => m.nonEmpty
    case it: Iterable[_] => it.nonEmpty
    case opt: Option[_] => opt.nonEmpty
    case _ => true
  }

  val customers = List[Auto1](Auto1(id = 1, name = "Alice"), Auto1(id = 2, name = "Bob"), Auto1(id = 3, name = "Charlie"), Auto1(id = 4, name = "Diana"))
  val orders = List[Auto2](Auto2(id = 100, customerId = 1, total = 250), Auto2(id = 101, customerId = 2, total = 125), Auto2(id = 102, customerId = 1, total = 300))
  val result = for { c <- customers; o = orders.find(o => (o.customerId).asInstanceOf[Int] == c.id) } yield Auto3(customerName = c.name, order = o)
  def main(args: Array[String]): Unit = {
    println(("--- Right Join using syntax ---"))
    for(entry <- result) {
      if (_truthy(entry.order)) {
        println(("Customer") + " " + (entry.customerName) + " " + ("has order") + " " + (entry.order.id) + " " + ("- $") + " " + (entry.order.total))
      } else {
        println(("Customer") + " " + (entry.customerName) + " " + ("has no orders"))
      }
    }
  }
}
