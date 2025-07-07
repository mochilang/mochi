object group_by_join {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int)
  case class Stats(name: String, count: Int)

  def main(args: Array[String]): Unit = {
    val customers = List(Customer(1, "Alice"), Customer(2, "Bob"))
    val orders = List(Order(100, 1), Order(101, 1), Order(102, 2))

    val joined = for {
      o <- orders
      c <- customers if o.customerId == c.id
    } yield (c.name)

    val stats = joined.groupBy(identity).map { case (name, list) => Stats(name, list.size) }

    println("--- Orders per customer ---")
    stats.foreach(s => println(s"${s.name} orders: ${s.count}"))
  }
}
