object inner_join {
  case class Auto1(id: Int, name: String)
  case class Auto2(id: Int, customerId: Int, total: Int)
  case class Auto3(orderId: Int, customerName: String, total: Int)

  val customers = List[Auto1](Auto1(id = 1, name = "Alice"), Auto1(id = 2, name = "Bob"), Auto1(id = 3, name = "Charlie"))
  val orders = List[Auto2](Auto2(id = 100, customerId = 1, total = 250), Auto2(id = 101, customerId = 2, total = 125), Auto2(id = 102, customerId = 1, total = 300), Auto2(id = 103, customerId = 4, total = 80))
  val result = for { o <- orders; c <- customers; if o.customerId == (c.id).asInstanceOf[Int] } yield Auto3(orderId = o.id, customerName = c.name, total = o.total)
  def main(args: Array[String]): Unit = {
    println(("--- Orders with customer info ---"))
    for(entry <- result) {
      println(("Order") + " " + (entry.orderId) + " " + ("by") + " " + (entry.customerName) + " " + ("- $") + " " + (entry.total))
    }
  }
}
