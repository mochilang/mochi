object group_by_left_join {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int)
  case class Stats(name: String, count: Int)

  def main(args: Array[String]): Unit = {
    val customers = List(
      Customer(1, "Alice"),
      Customer(2, "Bob"),
      Customer(3, "Charlie")
    )
    val orders = List(
      Order(100, 1),
      Order(101, 1),
      Order(102, 2)
    )

    val joined = for {
      c <- customers
      oOpt = orders.filter(_.customerId == c.id)
      o <- oOpt ++ (if (oOpt.isEmpty) List(null) else Nil)
    } yield (c.name, Option(o))

    val stats = joined.groupBy(_._1).map { case (name, list) =>
      val count = list.count(_._2.isDefined)
      Stats(name, count)
    }

    println("--- Group Left Join ---")
    stats.foreach(s => println(s"${s.name} orders: ${s.count}"))
  }
}
