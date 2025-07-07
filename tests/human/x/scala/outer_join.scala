object outer_join {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int, total: Int)
  case class Row(order: Option[Order], customer: Option[Customer])

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
      Order(102, 1, 300),
      Order(103, 5, 80)
    )

    val pairs = (for (o <- orders) yield Row(Some(o), customers.find(_.id == o.customerId))) ++
      (for (c <- customers if !orders.exists(_.customerId == c.id)) yield Row(None, Some(c)))

    println("--- Outer Join using syntax ---")
    pairs.foreach { row =>
      (row.order, row.customer) match {
        case (Some(o), Some(c)) => println(s"Order ${o.id} by ${c.name} - $${o.total}")
        case (Some(o), None)    => println(s"Order ${o.id} by Unknown - $${o.total}")
        case (None, Some(c))    => println(s"Customer ${c.name} has no orders")
        case _ =>
      }
    }
  }
}
