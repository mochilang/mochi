object right_join {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int, total: Int)
  case class Entry(customerName: String, order: Option[Order])

  def main(args: Array[String]): Unit = {
    val customers = List(
      Customer(1, "Alice"),
      Customer(2, "Bob"),
      Customer(3, "Charlie"),
      Customer(4, "Diana")
    )
    val orders = List(
      Order(100, 1, 250),
      Order(101, 2, 125),
      Order(102, 1, 300)
    )

    val result = for {
      c <- customers
    } yield Entry(c.name, orders.find(_.customerId == c.id))

    println("--- Right Join using syntax ---")
    result.foreach { e =>
      e.order match {
        case Some(o) => println(s"Customer ${e.customerName} has order ${o.id} - $${o.total}")
        case None    => println(s"Customer ${e.customerName} has no orders")
      }
    }
  }
}
