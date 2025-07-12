object left_join_multi {
  case class Customer(id: Int, name: String)
  case class Item(orderId: Int, sku: String)
  case class Order(id: Int, customerId: Int)
  case class Result(orderId: Int, name: String, item: Option[Item])
  case class Result1(orderId: Int, name: String, item: Item)

  val customers = List(Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"))
  val orders = List(Order(id = 100, customerId = 1), Order(id = 101, customerId = 2))
  val items = List(Item(orderId = 100, sku = "a"))
  val result = for { o <- orders; c <- customers; if o.customerId == (c.id).asInstanceOf[Int]; i = items.find(i => o.id == (i.orderId).asInstanceOf[Int]) } yield Result(orderId = o.id, name = c.name, item = i)
  def main(args: Array[String]): Unit = {
    println("--- Left Join Multi ---")
    for(r <- result) {
      println(r.orderId + " " + r.name + " " + r.item)
    }
  }
}
