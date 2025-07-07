object left_join {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int, total: Int)
  case class Entry(orderId: Int, customer: Option[Customer], total: Int)

  def main(args: Array[String]): Unit = {
    val customers = List(Customer(1, "Alice"), Customer(2, "Bob"))
    val orders = List(Order(100, 1, 250), Order(101, 3, 80))

    val result = for {
      o <- orders
      c = customers.find(_.id == o.customerId)
    } yield Entry(o.id, c, o.total)

    println("--- Left Join ---")
    result.foreach { e =>
      println(s"Order ${e.orderId} customer ${e.customer} total ${e.total}")
    }
  }
}
