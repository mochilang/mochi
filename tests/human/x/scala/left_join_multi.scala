object left_join_multi {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int)
  case class Item(orderId: Int, sku: String)
  case class Result(orderId: Int, name: String, item: Option[Item])

  def main(args: Array[String]): Unit = {
    val customers = List(Customer(1, "Alice"), Customer(2, "Bob"))
    val orders = List(Order(100, 1), Order(101, 2))
    val items = List(Item(100, "a"))

    val result = for {
      o <- orders
      c <- customers if o.customerId == c.id
      i = items.find(_.orderId == o.id)
    } yield Result(o.id, c.name, i)

    println("--- Left Join Multi ---")
    result.foreach(r => println(s"${r.orderId} ${r.name} ${r.item}"))
  }
}
