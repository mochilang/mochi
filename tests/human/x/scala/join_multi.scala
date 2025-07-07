object join_multi {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int)
  case class Item(orderId: Int, sku: String)
  case class Result(name: String, sku: String)

  def main(args: Array[String]): Unit = {
    val customers = List(Customer(1, "Alice"), Customer(2, "Bob"))
    val orders = List(Order(100, 1), Order(101, 2))
    val items = List(Item(100, "a"), Item(101, "b"))

    val result = for {
      o <- orders
      c <- customers if o.customerId == c.id
      i <- items if o.id == i.orderId
    } yield Result(c.name, i.sku)

    println("--- Multi Join ---")
    result.foreach(r => println(s"${r.name} bought item ${r.sku}"))
  }
}
