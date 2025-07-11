object cross_join {
  case class Auto1(id: Int, name: String)
  case class Auto2(id: Int, customerId: Int, total: Int)
  case class Auto3(orderId: Int, orderCustomerId: Int, pairedCustomerName: String, orderTotal: Int)

  val customers = List[Auto1](Auto1(id = 1, name = "Alice"), Auto1(id = 2, name = "Bob"), Auto1(id = 3, name = "Charlie"))
  val orders = List[Auto2](Auto2(id = 100, customerId = 1, total = 250), Auto2(id = 101, customerId = 2, total = 125), Auto2(id = 102, customerId = 1, total = 300))
  val result = for { o <- orders; c <- customers } yield Auto3(orderId = o.id, orderCustomerId = o.customerId, pairedCustomerName = c.name, orderTotal = o.total)
  def main(args: Array[String]): Unit = {
    println(("--- Cross Join: All order-customer pairs ---"))
    for(entry <- result) {
      println(("Order") + " " + (entry.orderId) + " " + ("(customerId:") + " " + (entry.orderCustomerId) + " " + (", total: $") + " " + (entry.orderTotal) + " " + (") paired with") + " " + (entry.pairedCustomerName))
    }
  }
}
