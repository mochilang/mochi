object left_join {
  val customers = List(Map("id" -> (1), "name" -> ("Alice")), Map("id" -> (2), "name" -> ("Bob")))
  val orders = List(Map("id" -> (100), "customerId" -> (1), "total" -> (250)), Map("id" -> (101), "customerId" -> (3), "total" -> (80)))
  val result = for { o <- orders; c = customers.find(c => (o.customerId).asInstanceOf[Int] == (c.id).asInstanceOf[Int]) } yield Map("orderId" -> (o("id")), "customer" -> (c), "total" -> (o("total")))
  def main(args: Array[String]): Unit = {
    println(("--- Left Join ---"))
    for(entry <- result) {
      println(("Order") + " " + (entry("orderId")) + " " + ("customer") + " " + (entry("customer")) + " " + ("total") + " " + (entry("total")))
    }
  }
}
