object right_join {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int, total: Int)
  case class Result(customerName: String, order: Option[Order])
  case class Result1(customerName: String, order: Order)

  val customers = List[Customer](Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"), Customer(id = 3, name = "Charlie"), Customer(id = 4, name = "Diana"))
  val orders = List[Order](Order(id = 100, customerId = 1, total = 250), Order(id = 101, customerId = 2, total = 125), Order(id = 102, customerId = 1, total = 300))
  val result = for { c <- customers; o = orders.find(o => (o.customerId).asInstanceOf[Int] == c.id) } yield Result(customerName = c.name, order = o)
  def main(args: Array[String]): Unit = {
    println(("--- Right Join using syntax ---"))
    for(entry <- result) {
      if (entry.order != null) {
        println(("Customer") + " " + (entry.customerName) + " " + ("has order") + " " + (entry.order.id) + " " + ("- $") + " " + (entry.order.total))
      } else {
        println(("Customer") + " " + (entry.customerName) + " " + ("has no orders"))
      }
    }
  }
}
