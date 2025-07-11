object cross_join {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int, total: Int)
  case class Result(orderId: Int, orderCustomerId: Int, pairedCustomerName: String, orderTotal: Int)

  val customers = List[Customer](Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"), Customer(id = 3, name = "Charlie"))
  val orders = List[Order](Order(id = 100, customerId = 1, total = 250), Order(id = 101, customerId = 2, total = 125), Order(id = 102, customerId = 1, total = 300))
  val result = for { o <- orders; c <- customers } yield Result(orderId = o.id, orderCustomerId = o.customerId, pairedCustomerName = c.name, orderTotal = o.total)
  def main(args: Array[String]): Unit = {
    println(("--- Cross Join: All order-customer pairs ---"))
    for(entry <- result) {
      println(("Order") + " " + (entry.orderId) + " " + ("(customerId:") + " " + (entry.orderCustomerId) + " " + (", total: $") + " " + (entry.orderTotal) + " " + (") paired with") + " " + (entry.pairedCustomerName))
    }
  }
}
