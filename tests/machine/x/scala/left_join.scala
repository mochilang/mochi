object left_join {
  case class Auto1(id: Int, name: String)
  case class Auto2(id: Int, customerId: Int, total: Int)
  case class Auto3(orderId: Int, customer: Auto1, total: Int)

  val customers = List[Auto1](Auto1(id = 1, name = "Alice"), Auto1(id = 2, name = "Bob"))
  val orders = List[Auto2](Auto2(id = 100, customerId = 1, total = 250), Auto2(id = 101, customerId = 3, total = 80))
  val result = for { o <- orders; c = customers.find(c => o.customerId == (c.id).asInstanceOf[Int]) } yield Auto3(orderId = o.id, customer = c, total = o.total)
  def main(args: Array[String]): Unit = {
    println(("--- Left Join ---"))
    for(entry <- result) {
      println(("Order") + " " + (entry.orderId) + " " + ("customer") + " " + (entry.customer) + " " + ("total") + " " + (entry.total))
    }
  }
}
