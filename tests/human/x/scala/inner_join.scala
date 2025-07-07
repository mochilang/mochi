object inner_join {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int, total: Int)
  case class Entry(orderId: Int, customerName: String, total: Int)

  def main(args: Array[String]): Unit = {
    val customers = List(
      Customer(1, "Alice"),
      Customer(2, "Bob"),
      Customer(3, "Charlie")
    )
    val orders = List(
      Order(100, 1, 250),
      Order(101, 2, 125),
      Order(102, 1, 300),
      Order(103, 4, 80)
    )

    val result = for {
      o <- orders
      c <- customers if o.customerId == c.id
    } yield Entry(o.id, c.name, o.total)

    println("--- Orders with customer info ---")
    result.foreach(e => println(s"Order ${e.orderId} by ${e.customerName} - $${e.total}"))
  }
}
