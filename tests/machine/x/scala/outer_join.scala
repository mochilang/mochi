object outer_join {
  val customers = List(Map("id" -> (1), "name" -> ("Alice")), Map("id" -> (2), "name" -> ("Bob")), Map("id" -> (3), "name" -> ("Charlie")), Map("id" -> (4), "name" -> ("Diana")))
  val orders = List(Map("id" -> (100), "customerId" -> (1), "total" -> (250)), Map("id" -> (101), "customerId" -> (2), "total" -> (125)), Map("id" -> (102), "customerId" -> (1), "total" -> (300)), Map("id" -> (103), "customerId" -> (5), "total" -> (80)))
  val result = for { o <- orders; c = customers.find(c => (o.customerId).asInstanceOf[Int] == (c.id).asInstanceOf[Int]) } yield Map("order" -> (o), "customer" -> (c))
  def main(args: Array[String]): Unit = {
    println(("--- Outer Join using syntax ---"))
    for(row <- result) {
      if ((row("order")).asInstanceOf[Boolean]) {
        if ((row("customer")).asInstanceOf[Boolean]) {
          println(("Order") + " " + (row("order")("id")) + " " + ("by") + " " + (row("customer")("name")) + " " + ("- $") + " " + (row("order")("total")))
        } else {
          println(("Order") + " " + (row("order")("id")) + " " + ("by") + " " + ("Unknown") + " " + ("- $") + " " + (row("order")("total")))
        }
      } else {
        println(("Customer") + " " + (row("customer")("name")) + " " + ("has no orders"))
      }
    }
  }
}
