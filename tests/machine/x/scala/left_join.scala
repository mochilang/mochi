object left_join {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int, total: Int)
  case class Result(orderId: Int, customer: Option[Customer], total: Int)
  case class Result1(orderId: Int, customer: Customer, total: Int)

  val customers = List[Customer](Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"))
  val orders = List[Order](Order(id = 100, customerId = 1, total = 250), Order(id = 101, customerId = 3, total = 80))
  val result = for { o <- orders; c = customers.find(c => o.customerId == (c.id).asInstanceOf[Int]) } yield Result(orderId = o.id, customer = c, total = o.total)
  def main(args: Array[String]): Unit = {
    println(("--- Left Join ---"))
    for(entry <- result) {
      println(("Order") + " " + (entry.orderId) + " " + ("customer") + " " + (entry.customer) + " " + ("total") + " " + (entry.total))
    }
  }
}
