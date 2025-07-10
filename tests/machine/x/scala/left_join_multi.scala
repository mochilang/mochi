object left_join_multi {
  case class Auto1(id: Int, name: String)
  case class Auto2(id: Int, customerId: Int)
  case class Auto3(orderId: Int, sku: String)
  case class Auto4(orderId: Int, name: String, item: Auto3)
  case class Auto5(orderId: Any, name: Any, item: Any)

  val customers = List[Auto1](Auto1(id = 1, name = "Alice"), Auto1(id = 2, name = "Bob"))
  val orders = List[Auto2](Auto2(id = 100, customerId = 1), Auto2(id = 101, customerId = 2))
  val items = List[Auto3](Auto3(orderId = 100, sku = "a"))
  val result = for { o <- orders; c <- customers; if o.customerId == (c.id).asInstanceOf[Int]; i = items.find(i => o.id == (i.orderId).asInstanceOf[Int]) } yield Auto4(orderId = o.id, name = c.name, item = i)
  def main(args: Array[String]): Unit = {
    println(("--- Left Join Multi ---"))
    for(r <- result) {
      println((r.orderId) + " " + (r.name) + " " + (r.item))
    }
  }
}
