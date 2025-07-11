object inner_join {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int, total: Int)
  case class Result(orderId: Int, customerName: String, total: Int)

  val customers = List[Customer](Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"), Customer(id = 3, name = "Charlie"))
  val orders = List[Order](Order(id = 100, customerId = 1, total = 250), Order(id = 101, customerId = 2, total = 125), Order(id = 102, customerId = 1, total = 300), Order(id = 103, customerId = 4, total = 80))
  val result = for { o <- orders; c <- customers; if o.customerId == (c.id).asInstanceOf[Int] } yield Result(orderId = o.id, customerName = c.name, total = o.total)
  def main(args: Array[String]): Unit = {
    println(("--- Orders with customer info ---"))
    for(entry <- result) {
      println(("Order") + " " + (entry.orderId) + " " + ("by") + " " + (entry.customerName) + " " + ("- $") + " " + (entry.total))
    }
  }
}
