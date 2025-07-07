object cross_join {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int, total: Int)
  case class Pair(orderId: Int, orderCustomerId: Int, pairedCustomerName: String, orderTotal: Int)

  def main(args: Array[String]): Unit = {
    val customers = List(
      Customer(1, "Alice"),
      Customer(2, "Bob"),
      Customer(3, "Charlie")
    )
    val orders = List(
      Order(100, 1, 250),
      Order(101, 2, 125),
      Order(102, 1, 300)
    )

    val result = for {
      o <- orders
      c <- customers
    } yield Pair(o.id, o.customerId, c.name, o.total)

    println("--- Cross Join: All order-customer pairs ---")
    result.foreach { entry =>
      println(s"Order ${entry.orderId} (customerId: ${entry.orderCustomerId}, total: $${entry.orderTotal}) paired with ${entry.pairedCustomerName}")
    }
  }
}

